from bigint import BigInt

# we can inline two BigInts like this
struct BigRat [2*sizeof[BigInt]] begin
    a:void {0};
    b:void {sizeof[BigInt]};
end

# and cast it when needed: (rat.a:BigInt)
