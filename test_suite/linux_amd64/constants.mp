export WORD, NULL, FULL, EMPTY, MAP, SLICE

# uma word tem 8 bytes, toda memória será alinhada dessa forma
const WORD = 8
const NULL = 0p

const FULL  = 1ss
const EMPTY = 0ss

const MAP   = 00000000_00000000_00000000_00001111ul
const SLICE = 00000000_00000000_00000000_11110000ul

proc main
begin 
end
