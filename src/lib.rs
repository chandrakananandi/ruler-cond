mod metrics;
use egg::*;
use indexmap::IndexMap;
use rand::{prelude::SliceRandom, Rng};
use rand_pcg::Pcg64;
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    fmt::Formatter,
    time::Duration,
    time::Instant,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Constant {
    Number(i32),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum ExprType {
    Number,
    Boolean,
    Invalid,
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Number(n) => write!(f, "{}", n),
            Constant::Boolean(b) => write!(f, "{}", b),
        }
    }
}

impl std::str::FromStr for Constant {
    type Err = String;
    fn from_str(s: &str) -> Result<Constant, Self::Err> {
        match s {
            "True" | "true" => Ok(Constant::Boolean(true)),
            "False" | "false" => Ok(Constant::Boolean(false)),
            _ => {
                if let Ok(n) = s.parse::<i32>() {
                    Ok(Constant::Number(n))
                } else {
                    Err(format!("'{}' is not a valid value for Constant", s))
                }
            }
        }
    }
}

define_language! {
    pub enum SimpleMath {
        "<>" = Neq([Id; 2]),
        "<=" = Leq([Id; 2]),
        ">=" = Geq([Id; 2]),
        "<" = Lt([Id; 2]),
        ">" = Gt([Id; 2]),
        "&" = And([Id; 2]),
        "|" = Or([Id; 2]),
        "+" = Add([Id; 2]),
        "-" = Sub([Id; 2]),
        "*" = Mul([Id; 2]),
        "/" = Div([Id; 2]),
        "~" = Neg(Id),
        "!" = Not(Id),
        Bool(bool),
        Num(i32),
        Var(egg::Symbol),
    }
}

#[derive(Default, Clone)]
pub struct SynthAnalysis {
    cvec_len: usize,
}

impl Analysis<SimpleMath> for SynthAnalysis {
    type Data = Vec<Option<Constant>>;

    fn merge(&self, to: &mut Self::Data, from: Self::Data) -> bool {
        // only do assertions on non-empty vecs
        if !to.is_empty() && !from.is_empty() {
            assert_eq!(to, &from);
        }
        false
    }

    fn make(egraph: &EGraph<SimpleMath, Self>, enode: &SimpleMath) -> Self::Data {
        // a closure to get the cvec for some eclass
        let x = |i: &Id| egraph[*i].data.iter().copied();
        let params = &egraph.analysis;
        match enode {
            SimpleMath::Num(n) => (0..params.cvec_len)
                .map(|_| Some(Constant::Number(*n)))
                .collect(),
            SimpleMath::Bool(b) => (0..params.cvec_len)
                .map(|_| Some(Constant::Boolean(*b)))
                .collect(),
            SimpleMath::Var(_) => vec![],
            SimpleMath::Not(a) => x(a)
                .map(|x| match x {
                    Some(Constant::Boolean(b)) => Some(Constant::Boolean(!b)),
                    _ => None,
                })
                .collect(),
            SimpleMath::Neg(a) => x(a)
                .map(|x| match x {
                    Some(Constant::Number(n)) => Some(Constant::Number(-n)),
                    _ => None,
                })
                .collect(),
            SimpleMath::And([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Boolean(b1)), Some(Constant::Boolean(b2))) => {
                        Some(Constant::Boolean(b1 && b2))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Or([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Boolean(b1)), Some(Constant::Boolean(b2))) => {
                        Some(Constant::Boolean(b1 || b2))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Neq([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x != y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
            SimpleMath::Leq([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x <= y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
            SimpleMath::Geq([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x >= y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
            SimpleMath::Lt([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x < y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
            SimpleMath::Gt([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| {
                    if x > y {
                        Some(Constant::Boolean(true))
                    } else {
                        Some(Constant::Boolean(false))
                    }
                })
                .collect(),
            SimpleMath::Add([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                        Some(Constant::Number(n1.wrapping_add(n2)))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Sub([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                        Some(Constant::Number(n1.wrapping_sub(n2)))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Mul([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                        Some(Constant::Number(n1.wrapping_mul(n2)))
                    }
                    (_, _) => None,
                })
                .collect(),
            SimpleMath::Div([a, b]) => x(a)
                .zip(x(b))
                .map(|(x, y)| match (x, y) {
                    (Some(Constant::Number(n1)), Some(Constant::Number(n2))) => {
                        if n2 != 0 {
                            Some(Constant::Number(n1 / n2))
                        } else {
                            None
                        }
                    }
                    (_, _) => None,
                })
                .collect(),
        }
    }

    fn modify(_: &mut EGraph<SimpleMath, Self>, _: Id) {}
}

fn generalize(expr: &RecExpr<SimpleMath>, map: &mut HashMap<Symbol, Var>) -> Pattern<SimpleMath> {
    let alpha = b"abcdefghijklmnopqrstuvwxyz";
    let nodes: Vec<_> = expr
        .as_ref()
        .iter()
        .map(|n| match n {
            SimpleMath::Var(sym) => {
                let var = if let Some(var) = map.get(&sym) {
                    *var
                } else {
                    let var = format!("?{}", alpha[map.len()] as char).parse().unwrap();
                    map.insert(*sym, var);
                    var
                };
                ENodeOrVar::Var(var)
            }
            n => ENodeOrVar::ENode(n.clone()),
        })
        .collect();

    Pattern::from(PatternAst::from(nodes))
}

fn pattern_has_pred(pattern: &Pattern<SimpleMath>) -> bool {
    use SimpleMath::*;
    let mut nodes = pattern.ast.as_ref().iter();
    nodes.any(|n| match n {
        ENodeOrVar::Var(_) => false,
        ENodeOrVar::ENode(en) => matches! (en, Neq(..) | And(..) | Or(..) | Bool(_) | Gt(..) | Geq(..) | Lt(..) | Leq(..) | Not(_))
        }
    )
}

pub struct SynthParam {
    pub rng: Pcg64,
    pub n_iter: usize,
    pub n_samples: usize,
    pub variables: Vec<egg::Symbol>,
    pub consts: Vec<Constant>,
    pub diff_thresh: usize,
}

impl SynthParam {
    fn mk_egraph(&mut self) -> EGraph<SimpleMath, SynthAnalysis> {
        let mut egraph = EGraph::new(SynthAnalysis {
            // for now just adding 0 and 1 forcefully to the cvecs for variables
            // to test conditional rules for division
            cvec_len: self.n_samples + self.consts.len(),
        });
        let rng = &mut self.rng;
        for var in &self.variables {
            let id = egraph.add(SimpleMath::Var(*var));
            let mut cvec: Vec<Option<Constant>> = (0..self.n_samples)
                .map(|_| Some(Constant::Number(rng.gen::<i32>())))
                .collect();
            cvec.push(Some(Constant::Number(0)));
            cvec.push(Some(Constant::Number(1)));
            cvec.shuffle(rng);
            egraph[id].data = cvec;
        }
        // implicit sumbumption order here
        for n in &self.consts {
            if let Constant::Number(num) = n {
                egraph.add(SimpleMath::Num(*num));
            }
        }
        egraph
    }

    fn learn_cond_rules(
        &self,
        conditional: bool,
        eg: &EGraph<SimpleMath, SynthAnalysis>,
        added: Vec<Id>,
    ) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        let ids: Vec<Id> = eg.classes().map(|c| c.id).collect();
        let mut extract = Extractor::new(&eg, AstSize);
        let mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>> = vec![];

        let only_diff_at_none_poses =
            |(pos, (x, y)), none_poses: Vec<usize>| none_poses.contains(&pos) || x == y;

        let all_false = |cvec: &Vec<Option<Constant>>| {
            cvec.iter().all(|v| v == &Some(Constant::Boolean(false)))
        };

        // all eclass cvecs and Ids
        let ec_datas: Vec<(Vec<Option<Constant>>, Id)> =
            eg.classes().cloned().map(|c| (c.data, c.id)).collect();

        let same_cvec = |v1: &Vec<Option<Constant>>, v2: &Vec<Option<Constant>>| {
            v1.iter().zip(v2.iter()).all(|(x, y)| match (x, y) {
                (Some(Constant::Boolean(true)), Some(Constant::Boolean(true))) => true,
                (Some(Constant::Boolean(false)), Some(Constant::Boolean(false))) => true,
                (_, _) => false,
            })
        };

        let no_free_vars =
            |cond_ec: &Id, expr1: RecExpr<SimpleMath>, expr2: RecExpr<SimpleMath>| {
                let mut extract = Extractor::new(&eg, AstSize);
                let mut c_vars: HashSet<&Symbol> = HashSet::new();
                let mut e_vars: HashSet<&Symbol> = HashSet::new();
                let c = extract.find_best(*cond_ec).1;

                c.as_ref().iter().for_each(|en| {
                    if let SimpleMath::Var(v) = en {
                        c_vars.insert(v);
                    }
                });
                expr1.as_ref().iter().for_each(|en| {
                    if let SimpleMath::Var(v) = en {
                        e_vars.insert(v);
                    }
                });
                expr2.as_ref().iter().for_each(|en| {
                    if let SimpleMath::Var(v) = en {
                        e_vars.insert(v);
                    }
                });
                c_vars.iter().all(|cv| e_vars.contains(cv))
            };

        if conditional {
            for &i in &ids {
                for &j in &ids {
                    let mut agreement_vec: Vec<Option<Constant>> = Vec::new();

                    if i != j
                        && eg[i].data != eg[j].data
                        && (eg[i].data.contains(&None) || eg[j].data.contains(&None))
                    {
                        // all indices where either cvecs have None
                        let i_nones: Vec<usize> = eg[i]
                            .data
                            .iter()
                            .zip(eg[j].data.iter())
                            .enumerate()
                            .filter(|(_, (x, y))| *x == &None || *y == &None)
                            .map(|(i, _)| i)
                            .collect();

                        // if the cvecs are same everywhere else, then consider for potential conditional rule
                        if eg[i].data.iter().zip(eg[j].data.iter()).enumerate().all(
                            |(i, (x, y))| only_diff_at_none_poses((i, (x, y)), i_nones.clone()), //&& i_nones.clone().len() <= self.diff_thresh
                        ) {
                            for idx in 0..eg[j].data.len() {
                                if i_nones.contains(&idx) {
                                    // cvecs disagree at None positions
                                    agreement_vec.push(Some(Constant::Boolean(false)));
                                } else {
                                    // cvecs agree elsewhere
                                    agreement_vec.push(Some(Constant::Boolean(true)));
                                }
                            }

                            let (_cost1, expr1) = extract.find_best(i);
                            let (_cost2, expr2) = extract.find_best(j);

                            let cond = match ec_datas.iter().find(|(ec_data, cond_id)| {
                                same_cvec(&ec_data, &agreement_vec)
                                    && !all_false(&ec_data)
                                    && no_free_vars(cond_id, expr1.clone(), expr2.clone())
                            }) {
                                None => None,
                                Some((_, id)) => Some(extract.find_best(*id).1),
                            };

                            //TODO: we will  likely need to do this, using inverse subsumption order
                            //let cond = most_specific(conds);

                            if cond.is_some() {
                                let names = &mut HashMap::default();
                                let c = generalize(&cond.clone().unwrap(), names);
                                let pat1 = generalize(&expr1, names);
                                let pat2 = generalize(&expr2, names);
                                if pattern_has_pred(&c)
                                    && !pattern_has_pred(&pat1)
                                    && !pattern_has_pred(&pat2)
                                {
                                    equalities.extend(Equality::new(pat1, pat2, Some(c)))
                                }
                                let names = &mut HashMap::default();
                                let c = generalize(&cond.clone().unwrap(), names);
                                let pat1 = generalize(&expr2, names);
                                let pat2 = generalize(&expr1, names);
                                if pattern_has_pred(&c.clone())
                                    && !pattern_has_pred(&pat1)
                                    && !pattern_has_pred(&pat2)
                                {
                                    equalities.extend(Equality::new(pat1, pat2, Some(c)))
                                }
                            }
                        }
                    }
                }
            }
        }
        return equalities;
    }

    pub fn update_clean_egraph(
        &mut self,
        tainted_eg: EGraph<SimpleMath, SynthAnalysis>,
        mut clean_eg: EGraph<SimpleMath, SynthAnalysis>,
    ) -> EGraph<SimpleMath, SynthAnalysis> {
        let clean_ids: Vec<Id> = clean_eg.classes().map(|ec| ec.id).collect();
        for id in clean_ids {
            clean_eg.union(id, tainted_eg.find(id));
        }
        return clean_eg;
    }

    pub fn run(
        &mut self,
        num_ops: usize,
        _conditional: bool,
    ) -> Vec<Equality<SimpleMath, SynthAnalysis>> {
        let mut equalities: Vec<Equality<SimpleMath, SynthAnalysis>> = vec![];
        let mut eg = self.mk_egraph();
        let mut my_ids: BTreeSet<Id> = eg.classes().map(|c| c.id).collect();

        let mut metrics = metrics::RulerProfile::new();

        for iter in 0..self.n_iter {
            my_ids = my_ids.into_iter().map(|id| eg.find(id)).collect();

            println!(
                "iter {} phase 1: adding ops over {} eclasses",
                iter,
                my_ids.len(),
            );
            let mut enodes_to_add = vec![];

            let before = Instant::now();
            for &i in &my_ids {
                for &j in &my_ids {
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Add([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Sub([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Mul([i, j]));
                    }
                    // if find_type(&eg, i) == ExprType::Number {
                    //     enodes_to_add.push(SimpleMath::Neg(i));
                    // }
                    /*
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Div([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Boolean {
                        enodes_to_add.push(SimpleMath::Not(i));
                    }
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Leq([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number
                        && find_type(&eg, j) == ExprType::Number
                    {
                        enodes_to_add.push(SimpleMath::Geq([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Boolean
                        && find_type(&eg, j) == ExprType::Boolean
                    {
                        enodes_to_add.push(SimpleMath::And([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Boolean
                        && find_type(&eg, j) == ExprType::Boolean
                    {
                        enodes_to_add.push(SimpleMath::Or([i, j]));
                    }
                    if find_type(&eg, i) == ExprType::Number {
                        enodes_to_add.push(SimpleMath::Neg(i));
                    }
                    if i != j {
                        if find_type(&eg, i) == ExprType::Number
                            && find_type(&eg, j) == ExprType::Number
                        {
                            enodes_to_add.push(SimpleMath::Neq([i, j]));
                        } else if find_type(&eg, i) == ExprType::Boolean
                            && find_type(&eg, j) == ExprType::Boolean
                        {
                            enodes_to_add.push(SimpleMath::Neq([i, j]));
                        }
                    }
                    if i != j {
                        if find_type(&eg, i) == ExprType::Number
                            && find_type(&eg, j) == ExprType::Number
                        {
                            enodes_to_add.push(SimpleMath::Lt([i, j]));
                        }
                    }
                    if i != j {
                        if find_type(&eg, i) == ExprType::Number
                            && find_type(&eg, j) == ExprType::Number
                        {
                            enodes_to_add.push(SimpleMath::Gt([i, j]));
                        }
                    } */
                }
            }

            for enode in enodes_to_add {
                my_ids.insert(eg.add(enode));
            }
            let adding_exprs = Instant::now().duration_since(before);

            let mut nloop = 0;
            loop {
                nloop += 1;
                let before = Instant::now();
                let mut tainted_eg = eg.clone();
                let cloning_pristine = Instant::now().duration_since(before);

                println!(
                    "iter {} phase 2: before running rules, tainted eg has: enodes = {}, eclasses = {}",
                    iter,
                    tainted_eg.total_size(),
                    tainted_eg.number_of_classes()
                );

                let before = Instant::now();
                let mut set = HashSet::new();
                equalities.retain(|eq| set.insert(eq.name.clone()));

                let rules = equalities
                    .iter()
                    .filter(|eq| eq.cond == None)
                    .map(|eq| &eq.rewrite);

                let clean_rules = Instant::now().duration_since(before);

                let runner: Runner<SimpleMath, SynthAnalysis, ()> =
                    Runner::new(tainted_eg.analysis.clone()).with_egraph(tainted_eg);

                let before = Instant::now();
                let before_eqsat_eclasses = eg.number_of_classes();
                let before_eqsat_enodes = eg.total_number_of_nodes();
                tainted_eg = runner
                    .with_time_limit(Duration::from_secs(20))
                    .with_node_limit(usize::MAX)
                    .with_iter_limit(5)
                    .with_scheduler(SimpleScheduler)
                    .run(rules)
                    .egraph;

                let tainted_eqsat = Instant::now().duration_since(before);

                let before = Instant::now();
                eg = self.update_clean_egraph(tainted_eg, eg);
                let update_pristine = Instant::now().duration_since(before);

                eg.rebuild();

                my_ids = my_ids.into_iter().map(|id| eg.find(id)).collect();

                println!(
                    "       phase 2: after running {} rules, tainted eg has enodes = {}, eclasses = {}",
                    &equalities.len(),
                    eg.total_size(),
                    eg.number_of_classes()
                );

                let before_cvec_eclasses = eg.number_of_classes();
                let before_cvec_enodes = eg.total_number_of_nodes();

                println!("iter {} phase 3: discover rules", iter);
                let before = Instant::now();
                let mut by_cvec_some: IndexMap<&Vec<Option<Constant>>, Vec<Id>> = IndexMap::new();
                for class in eg.classes() {
                    if !eg[class.id].data.contains(&None) && my_ids.contains(&class.id) {
                        // the ids corresponding to a specific cvec key in this hash map are for normal rewrites and can be unioned.
                        by_cvec_some
                            .entry(&eg[class.id].data)
                            .or_default()
                            .push(class.id);
                    }
                }
                let cvec_grouping = Instant::now().duration_since(before);

                println!(
                    "iter {} phase 3: number of cvec groups: {}",
                    iter,
                    by_cvec_some.len()
                );

                let pattern_cost = |pat: &RecExpr<SimpleMath>| {
                    let mut n_consts = 0;
                    let mut vars = HashSet::new();
                    for node in pat.as_ref() {
                        match node {
                            SimpleMath::Num(..) => n_consts += 1,
                            SimpleMath::Var(v) => {
                                vars.insert(v);
                            }
                            _ => (),
                        }
                    }
                    (-(vars.len() as isize), n_consts, pat.as_ref().len())
                };

                let mut extract = Extractor::new(&eg, AstSize);

                let before = Instant::now();
                let learn_a_rule: Duration;
                let after_cvec_eclasses: usize;
                let after_cvec_enodes: usize;
                let mut learned_rule = String::new();

                let best = by_cvec_some
                    .values_mut()
                    .filter(|ids| ids.len() > 1)
                    .map(|ids| {
                        let mut extracted: Vec<_> = ids
                            .iter()
                            .map(|i| {
                                let expr = extract.find_best(*i).1;
                                (pattern_cost(&expr), *i, expr)
                            })
                            .collect();
                        extracted.sort_by(|a, b| a.0.cmp(&b.0).reverse());
                        // return the two cheapest things
                        (extracted.pop().unwrap(), extracted.pop().unwrap())
                    })
                    .min_by_key(|((cost1, _, _), (cost2, _, _))| {
                        // cost1.max(cost2).clone()
                        (cost1.0 + cost2.0, cost1.1 + cost2.1, cost1.2 + cost2.2)
                    });

                if let Some(((_, id1, expr1), (_, id2, expr2))) = best {
                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr1, names);
                    let pat2 = generalize(&expr2, names);
                    if !pattern_has_pred(&pat1) && !pattern_has_pred(&pat2) {
                        if let Some(eq) = Equality::new(pat1, pat2, None) {
                            if equalities.iter().all(|e| e.name != eq.name) {
                                learned_rule = eq.name.clone();
                                equalities.push(eq)
                            }
                        }
                    }

                    let names = &mut HashMap::default();
                    let pat1 = generalize(&expr2, names);
                    let pat2 = generalize(&expr1, names);
                    if !pattern_has_pred(&pat1) && !pattern_has_pred(&pat2) {
                        if let Some(eq) = Equality::new(pat1, pat2, None) {
                            if equalities.iter().all(|e| e.name != eq.name) {
                                equalities.push(eq)
                            }
                        }
                    }
                    eg.union(id1, id2);
                    after_cvec_eclasses = eg.number_of_classes();
                    after_cvec_enodes = eg.total_number_of_nodes();
                    learn_a_rule = Instant::now().duration_since(before);
                } else {
                    break;
                }
                metrics.record(
                    iter,
                    adding_exprs,
                    nloop,
                    cloning_pristine,
                    clean_rules,
                    tainted_eqsat,
                    update_pristine,
                    cvec_grouping,
                    learn_a_rule,
                    before_eqsat_eclasses,
                    before_eqsat_enodes,
                    before_cvec_eclasses,
                    before_cvec_enodes,
                    after_cvec_eclasses,
                    after_cvec_enodes,
                    learned_rule,
                );
            }

            println!("After iter {}, I know these equalities:", iter);
            for eq in &equalities {
                println!("  {}", eq);
            }
        }
        metrics.print_to_file();
        // println!("eclasses: {}, hashcons: {}, enodes: {}, myids: {}", eg.number_of_classes(), eg.total_number_of_nodes(), eg.total_size(), my_ids.len());
        // let cond_rws = self.learn_cond_rules(_conditional, &eg, added);
        // equalities.extend(cond_rws);
        equalities
    }
}

pub struct Equality<L, A> {
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    pub cond: Option<Pattern<L>>,
    pub name: String,
    pub rewrite: egg::Rewrite<L, A>,
}

impl<L: Language, A> Display for Equality<L, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.rewrite.name())
    }
}

struct TypeBasedSearcher<F, S> {
    typefilter: F,
    searcher: S,
}

impl<F, S, L, A> egg::Searcher<L, A> for TypeBasedSearcher<F, S>
where
    L: Language,
    A: Analysis<L>,
    S: Searcher<L, A>,
    F: Fn(&EGraph<L, A>, Id) -> bool,
{
    fn search_eclass(&self, egraph: &EGraph<L, A>, eclass: Id) -> Option<SearchMatches> {
        if (self.typefilter)(egraph, eclass) {
            self.searcher.search_eclass(egraph, eclass)
        } else {
            None
        }
    }

    fn vars(&self) -> Vec<Var> {
        self.searcher.vars()
    }
}

fn find_type(eg: &EGraph<SimpleMath, SynthAnalysis>, id: Id) -> ExprType {
    match &eg[id].data[0] {
        Some(Constant::Number(_)) => ExprType::Number,
        Some(Constant::Boolean(_)) => ExprType::Boolean,
        None => ExprType::Invalid,
    }
}

impl Equality<SimpleMath, SynthAnalysis> {
    fn new(
        lhs: Pattern<SimpleMath>,
        rhs: Pattern<SimpleMath>,
        cond: Option<Pattern<SimpleMath>>,
    ) -> Option<Self> {
        if let Some(cond) = cond {
            let name = format!("{} => {} if {}", lhs, rhs, cond);
            // only run rules over non-predicate expressions since we already filter out predicate rules
            let f = |eg: &EGraph<_, SynthAnalysis>, id| find_type(eg, id) == ExprType::Number;
            let searcher = TypeBasedSearcher {
                typefilter: f,
                searcher: lhs.clone(),
            };

            let applier: ConditionalApplier<
                ConditionEqual<Pattern<SimpleMath>, Pattern<SimpleMath>>,
                Pattern<SimpleMath>,
            > = ConditionalApplier {
                applier: rhs.clone(),
                condition: ConditionEqual(cond.clone(), "true".parse().unwrap()),
            };

            let rw = egg::Rewrite::new(name.clone(), name.clone(), searcher, applier).ok()?;

            Some(Self {
                lhs,
                rhs,
                cond: Some(cond),
                name,
                rewrite: rw.clone(),
            })
        } else {
            let name = format!("{} => {}", lhs, rhs);
            // only run rules over non-predicate expressions since we already filter out predicate rules
            let f = |eg: &EGraph<_, SynthAnalysis>, id| find_type(eg, id) == ExprType::Number;
            let searcher = TypeBasedSearcher {
                typefilter: f,
                searcher: lhs.clone(),
            };

            let applier = rhs.clone();
            let rw = egg::Rewrite::new(name.clone(), name.clone(), searcher, applier).ok()?;

            Some(Self {
                lhs,
                rhs,
                cond,
                name,
                rewrite: rw,
            })
        }
    }
}

struct NoAddPatternApplier<L>(Pattern<L>);

impl<L, A> egg::Applier<L, A> for NoAddPatternApplier<L>
where
    L: Language,
    A: Analysis<L>,
{
    fn apply_one(&self, egraph: &mut EGraph<L, A>, _eclass: Id, subst: &Subst) -> Vec<Id> {
        let mut so_far: Vec<Id> = vec![];
        for node in self.0.ast.as_ref() {
            let id = match node {
                ENodeOrVar::ENode(n) => {
                    match egraph.lookup(n.clone().map_children(|i| so_far[usize::from(i)])) {
                        Some(id) => id,
                        None => return vec![],
                    }
                }
                ENodeOrVar::Var(v) => subst[*v],
            };
            so_far.push(id);
        }

        vec![*so_far.last().unwrap()]
    }

    fn vars(&self) -> Vec<Var> {
        self.0.vars()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::SeedableRng;

    fn check_proves<L, A>(eqs: &[Equality<L, A>], a: &str, b: &str)
    where
        L: Language + 'static,
        A: Analysis<L> + Default + 'static,
    {
        let mut rules: Vec<Rewrite<L, A>> = Vec::new();
        for eq in eqs {
            let l = eq.lhs.clone();
            let r = eq.rhs.clone();
            let rule = rewrite!(eq.name.clone(); l => r);
            rules.push(rule);
        }
        let runner: Runner<L, A, ()> = Runner::default()
            .with_expr(&a.parse().unwrap())
            .with_expr(&b.parse().unwrap())
            .with_hook(|runner| {
                if runner.egraph.find(runner.roots[0]) == runner.egraph.find(runner.roots[1]) {
                    Err(format!("Done early"))
                } else {
                    Ok(())
                }
            })
            .run(&rules);
        let id_a = runner.egraph.find(runner.roots[0]);
        let id_b = runner.egraph.find(runner.roots[1]);

        if id_a != id_b {
            panic!("Failed to simplify {} => {}, {}, {}", a, b, id_a, id_b);
        }
    }

    #[test]
    fn super_simple() {
        let mut param = SynthParam {
            rng: SeedableRng::seed_from_u64(5),
            n_iter: 1,
            n_samples: 25,
            variables: vec!["x".into(), "y".into(), "z".into()],
            consts: vec![Constant::Number(0), Constant::Number(1)],
            diff_thresh: 5,
        };

        let eqs = param.run(13, false);

        check_proves(&eqs, "(+ a b)", "(+ b a)");
        check_proves(&eqs, "(+ a 0)", "a");
        check_proves(&eqs, "(+ 0 a)", "a");
    }
}
