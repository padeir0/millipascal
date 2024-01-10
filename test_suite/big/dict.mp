from flalloc import Alloc, Free, Init

const NULL 0p

#   Hashmaps serão implementados como usualmente, usando uma array
# de ponteiros, onde cada ponteiro aponta pra uma lista encadeada
# de pares de chave-valor
# 
#  header
#  v
# [~|.| | | |.| | | ... ]
#    │       │      
#    v       v      
#    [ ]     [ ]    
#     └>[ ]   └>[ ] 
#                └>[ ]  
#
#   O Header do dicionário vai conter informação sobre o tamanho
# da array de buckets e o total de items inseridos, bem como vai manter
# um bitmap dos ponteiros na parte de valor do par de chave-valor, caso
# esse valor não seja uma união.

const MAP_HEADER_SIZE 24

# offset negativo
const MP_CAP_OFFSET 24
const MP_LEN_OFFSET 20
const MP_KSIZE_OFFSET 16
const MP_VSIZE_OFFSET 14
const MP_PROP_OFFSET 12
const MP_BITMAP_OFFSET 8

# Nó
# O ponteiro começa apontando pra chave, e o valor fica logo depois,
# o offset do valor é o tamanho do campo chave, mais algum padding
# 
# Chave vai ter 16 bytes e Valor vai ter 24 bytes
const NODE_K_OFFSET 0
const NODE_V_OFFSET 16
const NODE_NEXT_OFFSET 40

proc main
begin
end proc

proc NewMap[cap:u32, k_size:u16, v_size:u16,
            prop:u8, ptr_bitmap:u32] ptr
begin
    return NULL
end proc

# vamos admitir por agora que todas os objetos são
# essencialmente blobs de bytes, então faremos hash
# da chave com um algoritmo simples
proc hash[obj:ptr, size:i64] i64
var i, out
begin
    set i = 0
    while i < size begin
        set out = (out*31) |^ ((obj+i:ptr)@i8):i64
        set i += 1
    end while
    return out
end proc
