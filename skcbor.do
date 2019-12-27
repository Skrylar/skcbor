redo-ifchange skcbor.nim
exec 1>&2
nim c -o:$3 skcbor
./$3
