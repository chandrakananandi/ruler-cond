

# exit on error
set -e

# work in artifacts dir for intermediate files
mkdir -p enumo-artifacts
pushd enumo-artifacts > /dev/null

cat <<EOF > 256347020.rules
EOF


cat <<EOF > 149578203.workload
(plug "bop" {+ - *}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1}
        (filter (contains var)
          (filter (#atoms < 2)
            {lit var}))))))
EOF

if [ ! -f 149578203.terms ]; then
  enumo gen 149578203.workload 149578203.terms
fi

if [ ! -f 73958239.rules ]; then
  echo
  echo "Exploring #atoms 1"
  enumo convert -rules-to-ruler 256347020.rules 256347020.rules.json
  bigint synth  --prior-rules 256347020.rules.json --workload 149578203.terms --outfile 73958239.rules.json
  enumo convert -rules-of-ruler 73958239.rules.json 73958239.rules
fi

cat <<EOF > 2156278.workload
(plug "bop" {+ - *}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1}
        (filter (contains var)
          (filter (#atoms < 3)
            (plug "lang" {lit var}
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 2156278.terms ]; then
  enumo gen 2156278.workload 2156278.terms
fi

if [ ! -f 102338518.rules ]; then
  echo
  echo "Exploring #atoms 2"
  enumo convert -rules-to-ruler 73958239.rules 73958239.rules.json
  bigint synth  --prior-rules 73958239.rules.json --workload 2156278.terms --outfile 102338518.rules.json
  enumo convert -rules-of-ruler 102338518.rules.json 102338518.rules
fi

cat <<EOF > 761330878.workload
(plug "bop" {+ - *}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1}
        (filter (contains var)
          (filter (#atoms < 4)
            (plug "lang"
              (plug "lang" {lit var}
                {lit var (uop lang) (bop lang lang)})
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 761330878.terms ]; then
  enumo gen 761330878.workload 761330878.terms
fi

if [ ! -f 751718916.rules ]; then
  echo
  echo "Exploring #atoms 3"
  enumo convert -rules-to-ruler 102338518.rules 102338518.rules.json
  bigint synth  --prior-rules 102338518.rules.json --workload 761330878.terms --outfile 751718916.rules.json
  enumo convert -rules-of-ruler 751718916.rules.json 751718916.rules
fi

cat <<EOF > 379131871.workload
(plug "bop" {+ - *}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1}
        (filter (contains var)
          (filter (#atoms < 5)
            (plug "lang"
              (plug "lang"
                (plug "lang" {lit var}
                  {lit var (uop lang) (bop lang lang)})
                {lit var (uop lang) (bop lang lang)})
              {lit var (uop lang) (bop lang lang)})))))))
EOF

if [ ! -f 379131871.terms ]; then
  enumo gen 379131871.workload 379131871.terms
fi

if [ ! -f 1059566230.rules ]; then
  echo
  echo "Exploring #atoms 4"
  enumo convert -rules-to-ruler 751718916.rules 751718916.rules.json
  bigint synth  --prior-rules 751718916.rules.json --workload 379131871.terms --outfile 1059566230.rules.json
  enumo convert -rules-of-ruler 1059566230.rules.json 1059566230.rules
fi

cat <<EOF > 485098571.workload
(plug "bop" {+ - *}
  (plug "uop" {~}
    (plug "var" {a b c}
      (plug "lit" {-1 0 1}
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

if [ ! -f 485098571.terms ]; then
  enumo gen 485098571.workload 485098571.terms
fi

if [ ! -f 517306881.rules ]; then
  echo
  echo "Exploring #atoms 5"
  enumo convert -rules-to-ruler 1059566230.rules 1059566230.rules.json
  bigint synth  --prior-rules 1059566230.rules.json --workload 485098571.terms --outfile 517306881.rules.json
  enumo convert -rules-of-ruler 517306881.rules.json 517306881.rules
fi

popd > /dev/null

# copy rules to output path
cp enumo-artifacts/517306881.rules int-atoms5.rules

