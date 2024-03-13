from io import write

type Node is ptr^begin [sizeof[i8] + 2*sizeof[Node]]
    T:i32         {0};
    Contents:void {T+sizeof[T]};
end

type Leaf is ptr^i64

type Unary is ptr^begin
    Leaf:Node;
end

type Binary is ptr^begin
    Left:Node;
    Right:Node;
end

type Multiple is ptr^begin
    Len:i32;
    Cap:i32;
    Array:Node;
end

const begin
    # leaf
    NUM   = 0l;

    # unary
    NEG = 1l;

    # binary
    ADD   = 2l;
    SUB   = 3l;
    MULT  = 4l;
    DIV   = 5l;
    REM   = 6l;

    # multiple
    MAX   = 7l;
    MIN   = 8l;
end


data M:Base []

proc main
begin
    if eval[make_expr[]] != ~3 begin
        exit 1ss;
    end
end

proc eval[n:Node] i64
var bin:Binary, un:Unary, leaf:Leaf
begin
    if n->T == NUM begin
        set leaf = (n.Contents):Leaf;
        return @leaf;
    end elseif n->T == NEG begin
        set un = (n.Contents):Unary;
        return ~eval[un->Leaf];
    end elseif n->T == ADD begin
        set bin = (n.Contents):Binary;
        return eval[bin->Left] + eval[bin->Right];
    end elseif n->T == SUB begin
        set bin = (n.Contents):Binary;
        return eval[bin->Left] - eval[bin->Right];
    end elseif n->T == MULT begin
        set bin = (n.Contents):Binary;
        return eval[bin->Left] * eval[bin->Right];
    end elseif n->T == DIV begin
        set bin = (n.Contents):Binary;
        return eval[bin->Left] / eval[bin->Right];
    end elseif n->T == REM begin
        set bin = (n.Contents):Binary;
        return eval[bin->Left] % eval[bin->Right];
    end elseif n->T == MAX begin
        return max_of[n];
    end elseif n->T == MIN begin
        return min_of[n];
    end
end

data INVALID_EXPR "invalid expression\n"

proc min_of[n:Node] i64
var mult:Multiple, curr, min:i64, i:i32
begin
    set min = 1<<62;
    set i = 0l;
    set mult = (n.Contents):Multiple;
    if mult->Len == 0 begin
        write[INVALID_EXPR, sizeof[INVALID_EXPR]];
        exit 1ss;
    end
    while i < mult->Len begin
        set curr = eval[mult->Array.[i]];
        if curr < min begin
            set min = curr;
        end
        set i += 1;
    end
    return min;
end

proc max_of[n:Node] i64
var mult:Multiple, curr, max:i64, i:i32
begin
    set max = ~(1<<62);
    set i = 0l;
    set mult = (n.Contents):Multiple;
    if mult->Len == 0 begin
        write[INVALID_EXPR, sizeof[INVALID_EXPR]];
        exit 1ss;
    end
    while i < mult->Len begin
        set curr = eval[mult->Array.[i]];
        if curr > max begin
            set max = curr;
        end
        set i += 1;
    end
    return max;
end

# ~5+2*(max[1, 2, 3]/min[3, 5, 7]) == ~3

data Numbers:Node [5]

data Max:Node []
data Min:Node []

data Mult:Node []
data Div:Node []
data Neg:Node []
data Add:Node []

proc make_expr[] Node
var mult:Multiple, bin:Binary, un:Unary
begin
    set_num[Numbers.[0], 1];
    set_num[Numbers.[1], 2];
    set_num[Numbers.[2], 3];
    set_num[Numbers.[3], 5];
    set_num[Numbers.[4], 7];

    set Max->T = MAX;
    set mult = (Max.Contents):Multiple;
    set mult->Cap = 3;
    set mult->Len = 3;
    set mult->Array = Numbers.[0];

    set Min->T = MIN;
    set mult = (Min.Contents):Multiple;
    set mult->Cap = 3;
    set mult->Len = 3;
    set mult->Array = Numbers.[2];

    set Div->T = DIV;
    set bin = (Div.Contents):Binary;
    set bin->Left = Max;
    set bin->Right = Min;

    set Mult->T = MULT;
    set bin = (Mult.Contents):Binary;
    set bin->Left = Numbers.[1];
    set bin->Right = Div;

    set Neg->T = NEG;
    set un = (Neg.Contents):Unary;
    set un->Leaf = Numbers.[3];

    set Add->T = ADD;
    set bin = (Add.Contents):Binary;
    set bin->Left = Neg;
    set bin->Right = Mult;

    return Add;
end

proc set_num[m:Node, num:i64]
var cont:Leaf
begin
    set m->T = NUM;
    set cont = (m.Contents):Leaf;
    set @cont = num;
end
