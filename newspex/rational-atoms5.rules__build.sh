

# exit on error
set -e

# work in artifacts dir for intermediate files
mkdir -p enumo-artifacts
pushd enumo-artifacts > /dev/null

cat <<EOF > 256347020.rules
EOF


cat <<EOF > 736090004.workload
(plug "bop" {+ - * /}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1 2}
        (filter (contains var)
          (filter (#atoms < 2)
            {lit var}))))))
EOF

if [ ! -f 736090004.terms ]; then
  enumo gen 736090004.workload 736090004.terms
fi

if [ ! -f 771660241.rules ]; then
  echo
  echo "Exploring #atoms 1"
  enumo convert -rules-to-ruler 256347020.rules 256347020.rules.json
  rational synth  --prior-rules 256347020.rules.json --workload 736090004.terms --outfile 771660241.rules.json
  enumo convert -rules-of-ruler 771660241.rules.json 771660241.rules
fi

cat <<EOF > 657081715.workload
(plug "bop" {+ - * /}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1 2}
        (filter (contains var)
          (filter (#atoms < 3)
            (plug "lang" {lit var}
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 657081715.terms ]; then
  enumo gen 657081715.workload 657081715.terms
fi

if [ ! -f 384779449.rules ]; then
  echo
  echo "Exploring #atoms 2"
  enumo convert -rules-to-ruler 771660241.rules 771660241.rules.json
  rational synth  --prior-rules 771660241.rules.json --workload 657081715.terms --outfile 384779449.rules.json
  enumo convert -rules-of-ruler 384779449.rules.json 384779449.rules
fi

cat <<EOF > 7886111.workload
(plug "bop" {+ - * /}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1 2}
        (filter (contains var)
          (filter (#atoms < 4)
            (plug "lang"
              (plug "lang" {lit var}
                {lit var (uop lang) (bop lang lang)})
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 7886111.terms ]; then
  enumo gen 7886111.workload 7886111.terms
fi

if [ ! -f 866158991.rules ]; then
  echo
  echo "Exploring #atoms 3"
  enumo convert -rules-to-ruler 384779449.rules 384779449.rules.json
  rational synth  --prior-rules 384779449.rules.json --workload 7886111.terms --outfile 866158991.rules.json
  enumo convert -rules-of-ruler 866158991.rules.json 866158991.rules
fi

cat <<EOF > 496411349.workload
(plug "bop" {+ - * /}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1 2}
        (filter (contains var)
          (filter (#atoms < 5)
            (plug "lang"
              (plug "lang"
                (plug "lang" {lit var}
                  {lit var (uop lang) (bop lang lang)})
                {lit var (uop lang) (bop lang lang)})
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 496411349.terms ]; then
  enumo gen 496411349.workload 496411349.terms
fi

if [ ! -f 647440429.rules ]; then
  echo
  echo "Exploring #atoms 4"
  enumo convert -rules-to-ruler 866158991.rules 866158991.rules.json
  rational synth  --prior-rules 866158991.rules.json --workload 496411349.terms --outfile 647440429.rules.json
  enumo convert -rules-of-ruler 647440429.rules.json 647440429.rules
fi

cat <<EOF > 618899195.workload
(plug "bop" {+ - * /}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1 2}
        (filter (contains var)
          (filter (#atoms < 6)
            (plug "lang"
              (plug "lang"
                (plug "lang"
                  (plug "lang" {lit var}
                    {lit var (uop lang) (bop lang lang)})
                  {lit var (uop lang) (bop lang lang)})
                {lit var (uop lang) (bop lang lang)})
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 618899195.terms ]; then
  enumo gen 618899195.workload 618899195.terms
fi

if [ ! -f 173932829.rules ]; then
  echo
  echo "Exploring #atoms 5"
  enumo convert -rules-to-ruler 647440429.rules 647440429.rules.json
  rational synth  --prior-rules 647440429.rules.json --workload 618899195.terms --outfile 173932829.rules.json
  enumo convert -rules-of-ruler 173932829.rules.json 173932829.rules
fi

popd > /dev/null

# copy rules to output path
cp enumo-artifacts/173932829.rules rational-atoms5.rules

