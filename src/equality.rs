use egg::{Analysis, Language};

use crate::*;

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

pub fn find_type(eg: &EGraph<SimpleMath, SynthAnalysis>, id: Id) -> ExprType {
    match &eg[id].data[0] {
        Some(Constant::Number(_)) => ExprType::Number,
        Some(Constant::Boolean(_)) => ExprType::Boolean,
        None => ExprType::Invalid,
    }
}

impl Equality<SimpleMath, SynthAnalysis> {
    pub fn new(
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
