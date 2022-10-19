

# exit on error
set -e

# work in artifacts dir for intermediate files
mkdir -p enumo-artifacts
pushd enumo-artifacts > /dev/null

cat <<EOF > 256347020.rules
EOF


cat <<EOF > 1054417556.workload
(plug "bop" {& | ^ ->}
  (plug "uop" {~}
    (plug "var" {ba bb bc}
      (plug "lit" {true false}
        (filter (contains var)
          (filter (#atoms < 2)
            {lit var}))))))
EOF

if [ ! -f 1054417556.terms ]; then
  enumo gen 1054417556.workload 1054417556.terms
fi

if [ ! -f 699828815.rules ]; then
  echo
  echo "Exploring #atoms 1"
  enumo convert -rules-to-ruler 256347020.rules 256347020.rules.json
  bool synth --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --prior-rules 256347020.rules.json --workload 1054417556.terms --outfile 699828815.rules.json
  enumo convert -rules-of-ruler 699828815.rules.json 699828815.rules
fi

cat <<EOF > 9200891.workload
(plug "bop" {& | ^ ->}
  (plug "uop" {~}
    (plug "var" {ba bb bc}
      (plug "lit" {true false}
        (filter (contains var)
          (filter (#atoms < 3)
            (plug "lang" {lit var}
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 9200891.terms ]; then
  enumo gen 9200891.workload 9200891.terms
fi

if [ ! -f 1054349833.rules ]; then
  echo
  echo "Exploring #atoms 2"
  enumo convert -rules-to-ruler 699828815.rules 699828815.rules.json
  bool synth --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --prior-rules 699828815.rules.json --workload 9200891.terms --outfile 1054349833.rules.json
  enumo convert -rules-of-ruler 1054349833.rules.json 1054349833.rules
fi

cat <<EOF > 738555298.workload
(plug "bop" {& | ^ ->}
  (plug "uop" {~}
    (plug "var" {ba bb bc}
      (plug "lit" {true false}
        (filter (contains var)
          (filter (#atoms < 4)
            (plug "lang"
              (plug "lang" {lit var}
                {lit var (uop lang) (bop lang lang)})
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 738555298.terms ]; then
  enumo gen 738555298.workload 738555298.terms
fi

if [ ! -f 664048256.rules ]; then
  echo
  echo "Exploring #atoms 3"
  enumo convert -rules-to-ruler 1054349833.rules 1054349833.rules.json
  bool synth --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --prior-rules 1054349833.rules.json --workload 738555298.terms --outfile 664048256.rules.json
  enumo convert -rules-of-ruler 664048256.rules.json 664048256.rules
fi

cat <<EOF > 1008926864.workload
(filter (canon a b c)
  (plug "bop" {& | ^ ->}
    (plug "uop" {~}
      (plug "var" {ba bb bc}
        (plug "lit" {true false}
          (filter (contains var)
            (filter (#atoms < 5)
              (plug "lang"
                (plug "lang"
                  (plug "lang" {lit var}
                    {lit var (uop lang) (bop lang lang)})
                  {lit var (uop lang) (bop lang lang)})
                {lit var (uop lang) (bop lang lang)}))))))))
EOF

if [ ! -f 1008926864.terms ]; then
  enumo gen 1008926864.workload 1008926864.terms
fi

if [ ! -f 47437987.rules ]; then
  echo
  echo "Exploring #atoms 4"
  enumo convert -rules-to-ruler 664048256.rules 664048256.rules.json
  bool synth --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --prior-rules 664048256.rules.json --workload 1008926864.terms --outfile 47437987.rules.json
  enumo convert -rules-of-ruler 47437987.rules.json 47437987.rules
fi

cat <<EOF > 418552302.workload
(filter (canon a b c)
  (plug "bop" {& | ^ ->}
    (plug "uop" {~}
      (plug "var" {ba bb bc}
        (plug "lit" {true false}
          (filter (contains var)
            (filter (#atoms < 6)
              (plug "lang"
                (plug "lang"
                  (plug "lang"
                    (plug "lang" {lit var}
                      {lit var (uop lang) (bop lang lang)})
                    {lit var (uop lang) (bop lang lang)})
                  {lit var (uop lang) (bop lang lang)})
                {lit var (uop lang) (bop lang lang)}))))))))
EOF

if [ ! -f 418552302.terms ]; then
  enumo gen 418552302.workload 418552302.terms
fi

if [ ! -f 906925582.rules ]; then
  echo
  echo "Exploring #atoms 5"
  enumo convert -rules-to-ruler 47437987.rules 47437987.rules.json
  bool synth --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --prior-rules 47437987.rules.json --workload 418552302.terms --outfile 906925582.rules.json
  enumo convert -rules-of-ruler 906925582.rules.json 906925582.rules
fi

popd > /dev/null

# copy rules to output path
cp enumo-artifacts/906925582.rules bool-atoms5.rules

