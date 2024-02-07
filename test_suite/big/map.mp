# Cada objeto de metadados tem 32 bytes, separados da seguinte forma:
#   - 4 bytes: tamanho da chave
#   - 4 bytes: tamanho do valor
#   - 8 bytes: ponteiro para procedimento de hash
#   - 8 bytes: ponteiro para procedimento de comparação
#   - 8 bytes: ponteiro para procedimento de cópia

const M_KEYSIZE_OFFSET 0
const M_VALSIZE_OFFSET 4
const M_HASHPROC_OFFSET 8
const M_COMPPROC_OFFSET 16
const M_COPYPROC_OFFSET 24

proc main
begin
end proc
