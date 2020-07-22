module=skcbor

t:
    mkdir t

t/$module.t: t $module.nim
    nim c -o:$target $module.nim

check:V: t/$module.t
    prove

