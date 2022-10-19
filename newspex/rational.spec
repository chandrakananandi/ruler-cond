import "../common.spec"

let rat_opts = ""

let consts = { -1 0 1 2 }

let plug_rat = plug_lang consts vars { ~ } { + - * / }

let iter_rat met n =
  lang
    |> iter_metric met "lang" n
    |> filter (contains var)
    |> plug_rat

letrec rat_rules met n =
  let  msg =
    "Exploring " @ to_string met @ " " @ to_string n
  in
  if n <= 1 then
    ruler msg "rational" rat_opts <{}> (iter_rat met 1)
  else
    ruler msg "rational" rat_opts
      (rat_rules met (n - 1))
      (iter_rat met n)

ruleset rational-atoms5.rules = rat_rules #atoms 5
ruleset rational-lists1.rules = rat_rules #lists 1

let add = {
  (+ add add)
  (- add add)
  var
}

let mul = {
  (* mul mul)
  (/ mul mul)
  var
}

let mul-of-add =
  mul
    |> plug "mul" add
    |> plug "add" { var }
    |> plug "var" { a b c }

let add-of-mul =
  add
    |> plug "add" mul
    |> plug "mul" { var }
    |> plug "var" { a b c }

let rational-atoms5 = iter_rat #atoms 5 |> filter (canon a b c)
let rational-lists1 = iter_rat #lists 1 |> filter (canon a b c)