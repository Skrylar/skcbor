module=skcbor

t:
    mkdir t

t/$module.t: t $module.nim
    nim c -o:$target $module.nim

check:V: t/$module.t
    prove

docs:V:
    pandoc docs/readme.md -t gfm > README.md

push:V:
    git push github

