import "../common.spec"

let int_opts = ""

let consts = { -1 0 1 }

let plug_int = plug_lang consts vars { ~ } { + - * }

let iter_int met n =
  lang
    |> iter_metric met "lang" n
    |> filter (contains var)
    |> plug_int

letrec int_rules met n =
  let  msg =
    "Exploring " @ to_string met @ " " @ to_string n
  in
  if n <= 1 then
    ruler msg "bigint" int_opts <{}> (iter_int met 1)
  else
    ruler msg "bigint" int_opts
      (int_rules met (n - 1))
      (iter_int met n)

ruleset int-atoms5.rules = int_rules #atoms 5
ruleset int-lists2.rules = int_rules #lists 2