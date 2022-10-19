

# exit on error
set -e

# work in artifacts dir for intermediate files
mkdir -p enumo-artifacts
pushd enumo-artifacts > /dev/null

cat <<EOF > 256347020.rules
EOF


cat <<EOF > 360151464.workload
(plug "bop" {& | ^ ->}
  (plug "uop" {~}
    (plug "var" {ba bb bc}
      (plug "lit" {true false}
        (filter (contains var)
          (filter (#lists < 2)
            (plug "lang" {lit var}
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 360151464.terms ]; then
  enumo gen 360151464.workload 360151464.terms
fi

if [ ! -f 586126672.rules ]; then
  echo
  echo "Exploring #lists 1"
  enumo convert -rules-to-ruler 256347020.rules 256347020.rules.json
  bool synth --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --prior-rules 256347020.rules.json --workload 360151464.terms --outfile 586126672.rules.json
  enumo convert -rules-of-ruler 586126672.rules.json 586126672.rules
fi

cat <<EOF > 769800348.workload
(plug "bop" {& | ^ ->}
  (plug "uop" {~}
    (plug "var" {ba bb bc}
      (plug "lit" {true false}
        (filter (contains var)
          (filter (#lists < 3)
            (plug "lang"
              (plug "lang" {lit var}
                {lit var (uop lang) (bop lang lang)})
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 769800348.terms ]; then
  enumo gen 769800348.workload 769800348.terms
fi

if [ ! -f 559842741.rules ]; then
  echo
  echo "Exploring #lists 2"
  enumo convert -rules-to-ruler 586126672.rules 586126672.rules.json
  bool synth --eqsat-iter-limit 3 --eqsat-node-limit 2000000 --eqsat-time-limit 150 --prior-rules 586126672.rules.json --workload 769800348.terms --outfile 559842741.rules.json
  enumo convert -rules-of-ruler 559842741.rules.json 559842741.rules
fi

popd > /dev/null

# copy rules to output path
cp enumo-artifacts/559842741.rules bool-lists2.rules

