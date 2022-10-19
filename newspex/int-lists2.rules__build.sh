

# exit on error
set -e

# work in artifacts dir for intermediate files
mkdir -p enumo-artifacts
pushd enumo-artifacts > /dev/null

cat <<EOF > 256347020.rules
EOF


cat <<EOF > 979559719.workload
(plug "bop" {+ - *}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1}
        (filter (contains var)
          (filter (#lists < 2)
            (plug "lang" {lit var}
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 979559719.terms ]; then
  enumo gen 979559719.workload 979559719.terms
fi

if [ ! -f 734797829.rules ]; then
  echo
  echo "Exploring #lists 1"
  enumo convert -rules-to-ruler 256347020.rules 256347020.rules.json
  bigint synth  --prior-rules 256347020.rules.json --workload 979559719.terms --outfile 734797829.rules.json
  enumo convert -rules-of-ruler 734797829.rules.json 734797829.rules
fi

cat <<EOF > 485250522.workload
(plug "bop" {+ - *}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1}
        (filter (contains var)
          (filter (#lists < 3)
            (plug "lang"
              (plug "lang" {lit var}
                {lit var (uop lang) (bop lang lang)})
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 485250522.terms ]; then
  enumo gen 485250522.workload 485250522.terms
fi

if [ ! -f 958691133.rules ]; then
  echo
  echo "Exploring #lists 2"
  enumo convert -rules-to-ruler 734797829.rules 734797829.rules.json
  bigint synth  --prior-rules 734797829.rules.json --workload 485250522.terms --outfile 958691133.rules.json
  enumo convert -rules-of-ruler 958691133.rules.json 958691133.rules
fi

popd > /dev/null

# copy rules to output path
cp enumo-artifacts/958691133.rules int-lists2.rules

