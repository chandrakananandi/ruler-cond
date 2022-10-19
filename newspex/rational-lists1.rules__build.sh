

# exit on error
set -e

# work in artifacts dir for intermediate files
mkdir -p enumo-artifacts
pushd enumo-artifacts > /dev/null

cat <<EOF > 256347020.rules
EOF


cat <<EOF > 333080142.workload
(plug "bop" {+ - * /}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1 2}
        (filter (contains var)
          (filter (#lists < 2)
            (plug "lang" {lit var}
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 333080142.terms ]; then
  enumo gen 333080142.workload 333080142.terms
fi

if [ ! -f 919336593.rules ]; then
  echo
  echo "Exploring #lists 1"
  enumo convert -rules-to-ruler 256347020.rules 256347020.rules.json
  rational synth  --prior-rules 256347020.rules.json --workload 333080142.terms --outfile 919336593.rules.json
  enumo convert -rules-of-ruler 919336593.rules.json 919336593.rules
fi

popd > /dev/null

# copy rules to output path
cp enumo-artifacts/919336593.rules rational-lists1.rules

