module SalsaTest

using Salsa
using Salsa: AbstractComponent, InputMap, InputScalar, @component, @input, @connect
using BenchmarkTools
using Profile
using Testy


###########################
# Simple API usage

Salsa.@derived function foofunc(db, x::Int, y::Vector{Int}) :: Int
    sum([1 + x , y...])
end

@testset "Simple API Usage" begin
    @derived function foofunc(db, x::Int, y::Vector{Int}) :: Int
        sum([1 + x , y...])
    end

    @derived f2(db::AbstractComponent) = 1

    Salsa.@component C begin end

    c = C()
    @test f2(c) == 1
    @test foofunc(c, 1, [1,1]) == 4

    Salsa.@component DefaultConstructorComponent begin
        Salsa.@input x :: Salsa.InputScalar{Int}
        DefaultConstructorComponent(r = Salsa.Runtime()) = new(r, InputScalar(r, 100))
    end
    @testset "custom constructor with default values" begin
        c = DefaultConstructorComponent()
        @test c.x[] == 100
        c.x[] = 1
        @test c.x[] == 1
    end
end

Salsa.@component Compiler begin
    Salsa.@input i::Salsa.InputMap{Int,Int}
end
Salsa.@component Workspace begin
    Salsa.@input i2::Salsa.InputScalar{Int}
    Salsa.@connect compiler::Compiler
end
@testset "connected components" begin
    w = Workspace()
    w.i2[] = 1
    w.i2[] = 2
    @test w.i2[] == 2
    @test w.compiler.runtime.current_revision == w.runtime.current_revision == 2

    Salsa.@component TestStorage begin
        Salsa.@connect workspace::Workspace
    end

    c = TestStorage()

    i = Salsa.InputMap{Int, Int}(Salsa.Runtime())
    @test collect(i) == []
    setindex!(i, 1, 10)
    @test i[10] == 1
    @test collect(i) == [10=>1]

    i = Salsa.InputScalar{Int}(Salsa.Runtime())
    i[] = 1
    @test i[] == 1

    c = TestStorage()

    count = 0  # For tracking caching
    Salsa.@derived function foofunc(c::Workspace, x::Int, y::Vector{Int}) :: Int
        count += 1
        sum([3 + x , y...])
    end
    foofunc(c.workspace, 1, [1,2])  # runs foofunc (and prints)
    foofunc(c.workspace, 1, [1,2])  # returns cached value
    foofunc(c.workspace, 3, [1,2])  # runs again
    @test count == 2
end

@testset "Multiple methods same function" begin
    @derived f(db::AbstractComponent)::Int = 1
    @derived f(db::AbstractComponent, x)::Int = x

    @component C1 begin end
    @test f(C1()) == 1
    @test f(C1(), 20) == 20
end


# ----  Example Usage Tests  ---------------------------------------------------------------

module Workspaces
using ..Salsa
Salsa.@component Workspace begin
    Salsa.@input relnames :: Salsa.InputScalar{Vector{Symbol}}
    Salsa.@input defs     :: Salsa.InputMap{Symbol, String}
end
end  # module Workspaces

@derived function alldefs(w::Workspaces.Workspace)
    join(w.defs[r] for r in w.relnames[])
end

w = Workspaces.Workspace()
w.relnames[] = []
@show alldefs(w)


############################
#
# Example usage
#
############################

const Manifest = Vector{String}

"Dummy `Ast` implementation"
struct Ast
    left
    right
    Ast(l=nothing,r=nothing) = new(l,r)
end

function extend(ast::Ast, piece::Ast)
    Ast(ast, piece)
end

function extend(ast::Ast, source::String)
    Ast(ast, source)
end

@testset "input parsing" begin
    @test @macroexpand(Salsa.@component MyQueryGroup begin
        # We can handle comments inside the Query Group just fine
        @input manifest :: Salsa.InputScalar{Manifest}
    end) isa Expr

    # Test expected parse errors:

    @test_throws Exception @macroexpand Salsa.@component MyQueryGroup begin
        Salsa.@input "input must be a function"
    end
    # Must be the _actual_ Salsa.@input macro
    @test_throws Exception @macroexpand Salsa.@component MyQueryGroup begin
        SomeOtherModule.@input manifest :: Salsa.InputScalar{Manifest}
    end

    # Other arbitrary macros are handled okay (For better or worse)
    @test @macroexpand(Salsa.@component MyQueryGroup begin
        Salsa.@input manifest :: Salsa.InputScalar{Manifest}

        @generated f(x) = x

        """ docstrings are technically a macro """
        MyQueryGroup() = 1
    end) isa Expr

    # @component and @derived macros support docstrings.
    @test @macroexpand(begin
        """ My Component """
        Salsa.@component MyQueryGroup begin
            Salsa.@input manifest :: Salsa.InputScalar{Manifest}
        end

        """ My derived function """
        Salsa.@derived function foo(db) end
    end) isa Expr
end

Salsa.@component MyQueryGroup begin
    @input manifest    :: Salsa.InputScalar{Manifest}
    @input source_text :: Salsa.InputMap{String, String}
end

Salsa.@derived function ast(db::MyQueryGroup, name::String)
    source = db.source_text[name]
    ast = Ast(source)
end

Salsa.@derived function whole_program_ast(db::MyQueryGroup)
    result = Ast()
    for filename in db.manifest[]
        result = extend(result, ast(db, filename))
    end
    return result
end

@testset "Salsa example (from Rust talks)" begin
    db = MyQueryGroup()

    @test isassigned(db.manifest) == false
    db.manifest[] = ["a.rs", "b.rs"]
    @test isassigned(db.manifest) == true

    @test length(db.source_text) == 0
    db.source_text["a.rs"] = "fn main() {}"
    db.source_text["b.rs"] = "fn bar();"
    @test haskey(db.source_text, "a.rs") == true
    @test haskey(db.source_text, "b.rs") == true
    @test haskey(db.source_text, "c.rs") == false

    @test db.manifest.v[] isa Some
    @test db.manifest.v[].value.changed_at == 1
    @test db.source_text.v["a.rs"].changed_at == 2
    @test db.source_text.v["b.rs"].changed_at == 3

    whole_program_ast(db)

    _ast_map = [d for d in values(db.runtime.derived_function_maps) if eltype(keys(d)) == Tuple{String}][1]
    @test _ast_map[("a.rs",)].changed_at == 3
    @test _ast_map[("a.rs",)].verified_at == 3
    @test _ast_map[("b.rs",)].changed_at == 3
    @test _ast_map[("b.rs",)].verified_at == 3

    _whole_program_ast_map = [d for d in values(db.runtime.derived_function_maps) if eltype(keys(d)) == Tuple{}][1]
    @test _whole_program_ast_map[()].changed_at == 3
    @test _whole_program_ast_map[()].verified_at == 3

    db.source_text["a.rs"] = "fn foo() {}"

    @test db.manifest.v[] isa Some
    @test db.manifest.v[].value.changed_at == 1
    @test db.source_text.v["a.rs"].changed_at == 4
    @test db.source_text.v["b.rs"].changed_at == 3

    whole_program_ast(db)

    @test _ast_map[("a.rs",)].changed_at == 4
    @test _ast_map[("a.rs",)].verified_at == 4
    @test _ast_map[("b.rs",)].changed_at == 3
    @test _ast_map[("b.rs",)].verified_at == 4

    @test _whole_program_ast_map[()].changed_at == 4
    @test _whole_program_ast_map[()].verified_at == 4

    # NOTE: users should not be calling `keys()` in derived queries, since it won't get added to dependency graph.
    filenames = sort([ name for name = keys(db.source_text) ])
    @test filenames == ["a.rs", "b.rs"]

    delete!(db.source_text, "a.rs")
    @test sort([ name for name = keys(db.source_text) ]) == ["b.rs"]
end

# ----------------------------------

Salsa.@component EarlyExitOptimizationTest begin
    Salsa.@input input :: Salsa.InputScalar{Int}
end
const ftrace = Set([])
Salsa.@derived function ispositive(db::EarlyExitOptimizationTest, x::Int) :: Bool
    push!(ftrace, ispositive)
    x > 0
end
Salsa.@derived function has_positive_input(db::EarlyExitOptimizationTest) :: Bool
    push!(ftrace, has_positive_input)
    ispositive(db, db.input[])
end
Salsa.@derived function match_input_sign(db::EarlyExitOptimizationTest, v::Int) :: Int
    push!(ftrace, match_input_sign)
    abs(v) * (has_positive_input(db) ? 1 : -1)
end

@testset "Early Exit Optimization Test" begin
    db = EarlyExitOptimizationTest()
    db.input[] = -1

    # First call runs all functions
    empty!(ftrace)
    @assert match_input_sign(db, 10) == -10
    @test ftrace == Set([match_input_sign, has_positive_input, ispositive])

    # Second call gets cached values, so only top-level is re-run
    empty!(ftrace)
    @assert match_input_sign(db, 5) == -5
    @test ftrace == Set([match_input_sign])

    # Calling again with the same value re-runs nothing.
    empty!(ftrace)
    @assert match_input_sign(db, 5) == -5
    @test ftrace == Set([])

    # _Setting_ the same value also doesn't dirty anything.
    db.input[] = -1
    empty!(ftrace)
    @assert match_input_sign(db, 5) == -5
    @test ftrace == Set([])

    # -----

    # If we set the input to a new value with opposite sign, all functions are re-run
    db.input[] = 1
    empty!(ftrace)
    @assert match_input_sign(db, 10) == 10
    @test ftrace == Set([match_input_sign, has_positive_input, ispositive])

    # -----

    # If we set the input to a new value with the _same_ sign, calling with the same value
    # stops at has_positive_input, because its return value is the same, and it terminates
    # early when it sees the same cached result.
    # Note that we must rerun both `ispositive` and `has_positive_input`, because
    # `has_positive_input` accesses the `input` _directly_. In this case, we gain almost
    # nothing from making `ispositive` a derived function, vs a normal julia function.
    db.input[] = 2
    empty!(ftrace)
    @assert match_input_sign(db, 10) == 10
    @test ftrace == Set([has_positive_input, ispositive])
end

# ----------------------------------

Salsa.@component ScalarValues begin
    Salsa.@input sv       :: Salsa.InputScalar{Int}
    Salsa.@input arrayval :: Salsa.InputScalar{Vector{Int}}
end

@testset "scalar values in QueryGroup" begin
    @testset "default scalar values?" begin
        # Reading an uninitialized InputScalar fails.
        @test_throws UndefRefError ScalarValues().sv[]
        @test_throws UndefRefError ScalarValues().arrayval[]
    end

    db = ScalarValues()

    # Assignment to the scalar value
    db.sv[] = 1
    @test db.sv[] == 1

    # Assignment to the scalar value
    db.arrayval[] = Int[]
    push!(db.arrayval[], 10)
    @test db.arrayval[] == [10]
end

# ----------------------------------

Salsa.@component ErrorHandling begin
    Salsa.@input v :: Salsa.InputScalar{Int}
end
Salsa.@derived function square_root(db::ErrorHandling)
    sqrt(db.v[])
end

@testset "Robust to queries that throw errors" begin
    db = ErrorHandling()

    # Setting a value that should work as expected
    db.v[] = 1
    @test square_root(db) == 1

    # Setting a value that will cause square_root() to throw an Exception
    db.v[] = -1
    @test_throws DomainError square_root(db)

    # Now test that it's recovered gracefully from the error, and we can still use the DB
    db.v[] = 1
    @test square_root(db) == 1  # ERROR: "Cycle in active query"
end

# ----------------------------------
Salsa.@component MapAggregatesDB begin
    Salsa.@input map::Salsa.InputMap{Int, Int}
end

Salsa.@derived function numelts(db::AbstractComponent)
    length(db.map)
end

Salsa.@derived mapvals(db::AbstractComponent) = sort(collect(values(db.map)))
Salsa.@derived mapkeys(db::AbstractComponent) = sort(collect(keys(db.map)))
Salsa.@derived function valsum(db::AbstractComponent)
    sum(values(db.map))
end
Salsa.@derived function keysum(db::AbstractComponent)
    sum(keys(db.map))
end
    # This actually loops over the elements, ∴ _touches_ them.
Salsa.@derived function looped_valsum(db::AbstractComponent)
    out = 0
    for (k,v) in db.map ; out += v ; end
    out
end

@testset "Operations over map keys" begin
    @testset "default map values?" begin
        @test length(MapAggregatesDB().map) == 0
        @test isempty(values(MapAggregatesDB().map))
        @test isempty(MapAggregatesDB().map)
    end

    @testset "keys over a map" begin
        db = MapAggregatesDB()
        db.map[1] = 10
        @test sort(collect(keys(db.map))) == [1]

        # Verify updates
        db.map[2] = 20
        @test sort(collect(keys(db.map))) == [1,2]
    end

    @testset "vals over a map" begin
        db = MapAggregatesDB()
        db.map[1] = 10
        @test collect(values(db.map)) == [10]

        # Now update the map and verify that the values do indeed update
        db.map[2] = 20
        # NOTE: keys(Salsa.Map) is currently not supported
        @test sort(collect(values(db.map))) == [10, 20]
    end
end

@testset "Map Inputs Aggregates" begin
    db = MapAggregatesDB()

    # Can call derived query without initializing the field
    @test numelts(db) == 0
    @test mapvals(db) == []
    @test valsum(db) == 0
    @test mapkeys(db) == []
    @test keysum(db) == 0
    # The version that loops over the (k,v) items, and therefore actually touches them.
    @test looped_valsum(db) == 0

    # If you update the values, the derived queries update.
    # NOTE: These are broken because currently _aggregate_ operations aren't dirtying the
    # Map's changed-at value
    db.map[1] = 10
    db.map[2] = 20
    @test_broken numelts(db) == 2
    @test_broken mapvals(db) == [10,20]
    @test_broken valsum(db) == 30
    @test_broken mapkeys(db) == [1,2]  # NOTE: keys is broken in general
    @test_broken keysum(db) == 3  # NOTE: keys is broken in general
    # NOTE: Surprisingly, even this isn't updated, which surprises me since it's similar
    # to the above `whole_program_ast` example.
    @test_broken looped_valsum(db) == 30
end

# ----------------------------------

# Allow functions with multiple methods
Salsa.@derived f(db::AbstractComponent) = 1
Salsa.@derived f(db::AbstractComponent, arg::Any) = arg
Salsa.@derived f(db::AbstractComponent, arg::Int) = 10

Salsa.@component MultiMethodFunctions begin
end
@testset "multiple methods" begin
    @test f(MultiMethodFunctions()) == 1
    @test f(MultiMethodFunctions(), 2) == 10
    @test f(MultiMethodFunctions(), "hi") == "hi"
end

# ----------------------------------


Salsa.@component AbstractTypes begin
    # Store abstract types in Input
    Salsa.@input in_scalar     :: Salsa.InputScalar{Any}
    Salsa.@input in_parametric :: Salsa.InputScalar{Vector}  # matches any kind of Vector
    Salsa.@input in_vec_of_any :: Salsa.InputScalar{Vector{Any}}  # only matches Vector{Any}
    Salsa.@input in_int_vector :: Salsa.InputScalar{Vector{Int}}  # Should be able to initialize with []

    # Abstract Keys and Values in maps
    Salsa.@input in_map_scalar     :: Salsa.InputMap{Any, Any}
    Salsa.@input in_map_parametric :: Salsa.InputMap{Vector, Vector}
    Salsa.@input in_map_vec_of_any :: Salsa.InputMap{Vector{Any}, Vector{Any}}
    Salsa.@input in_map_int_vector :: Salsa.InputMap{Vector{Int}, Vector{Int}}
end

# Control test case:
Salsa.@derived fully_typed_scalar(db::AbstractComponent) :: Int = 1

# Compute abstract types in derived  queries
Salsa.@derived derived_scalar(db::AbstractComponent) :: Any = 1
Salsa.@derived derived_parametric(db::AbstractComponent) :: Vector = [1,2,3]
Salsa.@derived derived_vec_of_any(db::AbstractComponent) :: Vector{Any} = [1,2,3]  # This will throw an Error

# Accept abstract params
Salsa.@derived derived_scalar_arg(db::AbstractComponent, arg::Any) :: Any = arg
Salsa.@derived derived_parametric_arg(db::AbstractComponent, arg::Vector) :: Vector = arg
Salsa.@derived derived_vec_of_any_arg(db::AbstractComponent, arg::Vector{Any}) :: Vector{Any} = arg

@testset "Returning AbstractTypes from QueryGroup" begin
    @testset "Assigning subtypes to Abstract Inputs" begin
        @test (AbstractTypes().in_scalar[] = 1; true)  # Assignment of subtype fails
        @test (AbstractTypes().in_parametric[] = [1,2,3]; true)

        # We should _probably_ be able to construct from a vector of Ints, since normal structs can:
        # ``` module M struct S x::Vector{Any} end end;   M.S([1,2])  # succeeds ```
        @test (AbstractTypes().in_vec_of_any[] = [1,2,3]; true)
        @test (AbstractTypes().in_vec_of_any[] = []; true)

        # Should be able to construct a Vector{Int} with `[]`
        @test (AbstractTypes().in_int_vector[] = []; true)
    end

    @testset "Assigning subtypes to Abstract Keys/Values in Inputs" begin
        @test (AbstractTypes().in_map_scalar[1] = 1; true)  # Assignment of subtype fails
        @test (AbstractTypes().in_map_parametric[[1,2,3]] = [1,2,3]; true)

        # We should _probably_ be able to construct from a vector of Ints, since normal structs can:
        # ``` module M struct S x::Vector{Any} end end;   M.S([1,2])  # succeeds ```
        @test (AbstractTypes().in_map_vec_of_any[[1,2,3]] = [1,2,3]; true)
        @test (AbstractTypes().in_map_vec_of_any[[]] = []; true)

        # Should be able to construct a Vector{Int} with `[]`
        @test (AbstractTypes().in_map_int_vector[[]] = []; true)
    end

    @testset "Baseline: Can @infer fully typed functions" begin
        @test @inferred(fully_typed_scalar(AbstractTypes())) == 1  # Returning subtype
    end

    @testset "Returning subtypes from derived functions" begin
        # NOTE that you cannot infer the results of these functions, because the map holds
        # an abstract type for the return value.
        @test derived_scalar(AbstractTypes()) == 1  # Returning subtype
        @test derived_parametric(AbstractTypes()) == [1,2,3]  # Returning subtype

        @test derived_vec_of_any(AbstractTypes()) == Any[1,2,3]  # Return value is `convert`able to Vector{Any}
    end

    @testset "Functions accept subtypes of Argument specifiers" begin
        @test derived_scalar_arg(AbstractTypes(), 1) == 1  # Pass subtype
        @test derived_parametric_arg(AbstractTypes(), [1,2,3]) == [1,2,3]  # Pass subtype

        @test_throws MethodError derived_vec_of_any_arg(AbstractTypes(), [1,2,3])  # Can't pass Vector{Int} to Vector{Any}
        @test derived_vec_of_any_arg(AbstractTypes(), Any[1,2,3]) == Any[1,2,3]  # Return value is `convert`able to Vecotr{Any}
    end
end

# ------------------------------------------

Salsa.@component MutableKeysValues begin
    Salsa.@input vec_to_int :: Salsa.InputMap{Vector{Int}, Int}
end

@testset "mutable keys" begin
    db = MutableKeysValues()
    a = [1,2,3]
    db.vec_to_int[a] = 1
    @assert db.vec_to_int[a] == 1
    @assert db.vec_to_int[[1,2,3]] == 1

    # Update `a` outside the db
    push!(a, 4)
    @test_throws KeyError db.vec_to_int[a]
    @test_broken db.vec_to_int[[1,2,3]] == 1   # Something about the equality test being broken
end

# --- Composed Component -------------------------------------

@component A begin
    @input x :: InputMap{Int,Int}
end
@component B begin
    @connect a :: A
end
@derived function b_a_x(b::B, k::Int)
    b.a.x[k]
end
@testset "composed component" begin
    b = B()
    b.a.x[1] = 10
    @test b_a_x(b, 1) == 10
end

# --- Abstract Component connection (circular dependency) ----------

module ExampleDatabaseModule
using ..Salsa
using ..Salsa: @connect
using Testy
# Defined before the Workspace
@component Compiler begin
    @input relation_defs :: InputMap{Int, String}
end
# `db` is expected to be Component that contains the Workspace (defined later to resolve
# circular dependency), and implements `get_workspace(db) :: Workspace`.
@derived function compile_relation(db::AbstractComponent, relid::Int) :: Int
    # Get the relation arity from the Workspace, which is defined after this Component.
    arity = get_workspace(db).relation_arity[relid]
    # ... for this example, just return the arity ...
    arity
end
@component Workspace begin
    @input relation_arity :: InputMap{Int, Int}

    @connect compiler :: Compiler
end
@component DatabaseStorage begin
    @connect workspace :: Workspace
end
function get_workspace(db::DatabaseStorage)
    db.workspace
end
@testset "composed component" begin
    db = DatabaseStorage()
    db.workspace.relation_arity[1] = 3
    @test compile_relation(db, 1) == 3
end
end

# --- Composed Component -------------------------------------

@component A begin
    @input x :: InputMap{Int,Int}
end
@component B begin
    @connect a :: A
end
@testset "composed component" begin
    b = B()
    b.a.x[1] = 10
    @test b.a.x[1] == 10
end



# --------------------------------------------------
# End of tests, start of benchmark
# --------------------------------------------------

const bench_scale = 10000

@noinline function create_bench_db()
    db = MyQueryGroup()

    man = ["$i" for i in 1:bench_scale]
    db.manifest[] = man
    for i in 1:bench_scale
        setindex!(db.source_text, "program $i {}", "$i")
    end

    return db
end

@noinline function incr_bench_db()
    db = create_bench_db()
    whole_program_ast(db)
    setindex!(db.source_text, "program 1 { }", "1")
    setindex!(db.source_text, "program $bench_scale { }", "$bench_scale")

    return db
end

function full_bench()
    b = @benchmarkable whole_program_ast(db) setup=(db = create_bench_db())
    return run(b)
end

function incr_bench()
    b = @benchmarkable whole_program_ast(db) setup=(db = incr_bench_db())
    return run(b)
end

function profile_run(n=10)
    Profile.clear()
    Profile.init(n=10^7)
    @profile for i in 1:n
        db = incr_bench_db()
        whole_program_ast(db)
    end
end

# ----------------------------------------------------------------------------
# Note: uncomment the lines below to profile the small Salsa benchmark
# ----------------------------------------------------------------------------

#profile_run(1)  # warmup
#@time profile_run()
#using PProf
#pprof(out="branch", from_c=false)

module M
using ..Salsa
@component Classroom begin
    @input student_grades :: InputMap{String, Float64}
end
@derived function letter_grade(c, name)
    println("computing grade for $name")
    ["D","C","B","A"][Int(round(c.student_grades[name]))]
end
c = Classroom()
c.student_grades["John"] = 3.25
letter_grade(c, "John")  # "B"; prints "computing grade for John"
letter_grade(c, "John")  # "B"; no output (uses cached value)
c.student_grades["John"] = 3.8
letter_grade(c, "John")  # "A"; prints "computing grade for John"


end

end
