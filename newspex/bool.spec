import "../common.spec"

let bool_opts =
  "--eqsat-iter-limit 3 "
  @@ "--eqsat-node-limit 2000000 "
  @@ "--eqsat-time-limit 150"

let vars = { ba bb bc }
let consts = { true false }

let plug_bool = plug_lang consts vars { ~ } { & | ^ -> }

let iter_bool met n = 
  lang
    |> iter_metric met "lang" n
    |> filter (contains var)
    |> plug_bool

letrec bool_rules met n =
  let  msg =
    "Exploring " @ to_string met @ " " @ to_string n
  in
  if n <= 1 then
    ruler msg "bool" bool_opts <{}> (iter_bool met 1)
  else if n < 4 then
    ruler msg "bool" bool_opts
      (bool_rules met (n - 1))
      (iter_bool met n)
  else
    ruler msg "bool" bool_opts
      (bool_rules met (n - 1))
      ((iter_bool met n) |> filter (canon a b c))

ruleset bool-atoms5.rules = bool_rules #atoms 5
ruleset bool-lists2.rules = bool_rules #lists 2