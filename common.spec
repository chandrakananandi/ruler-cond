let ruler msg domain opts = explore (
  "echo"
  @@@ "echo \"" @ msg @ "\""
  @@@ "enumo convert -rules-to-ruler "
        @@ <RULES_IN> @@ " "
        @@ <RULES_IN> @@ ".json"
  @@@ domain @@ " synth"
        @@ " " @@ opts
        @@ " --prior-rules " @@ <RULES_IN> @@ ".json"
        @@ " --workload " @@ <WORKLOAD>
        @@ " --outfile " @@ <RULES_OUT> @@ ".json"
  @@@ "enumo convert -rules-of-ruler "
        @@ <RULES_OUT> @@ ".json "
        @@ <RULES_OUT>
)

let lang = {
  lit
  var
  (uop lang)
  (bop lang lang)
}

let vars = { a b c }

let plug_lang lits vars uops bops wkld =
  wkld
    |> plug "lit" lits
    |> plug "var" vars
    |> plug "uop" uops
    |> plug "bop" bops
