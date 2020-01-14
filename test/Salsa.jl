module SalsaTest

import Salsa
using BenchmarkTools
using Profile
using Testy

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
    # TODO: This test is broken: we don't support comments in the @querygroup
    @test_broken @macroexpand Salsa.@querygroup MyQueryGroup begin
        # Currently can't handle comments inside the block
        @input function manifest(db) :: Manifest end
    end isa Expr

    # Test expected parse errors:

    @test_throws Exception @macroexpand Salsa.@querygroup MyQueryGroup begin
        Salsa.@input "input must be a function"
    end
    # Must be the _actual_ Salsa.@input macro
    @test_throws Exception @macroexpand Salsa.@querygroup MyQueryGroup begin
        SomeOtherModule.@input function manifest(db) :: Manifest end
    end
    # Other arbitrary macros aren't supported
    @test_throws Exception @macroexpand Salsa.@querygroup MyQueryGroup begin
        @generated function manifest(db) :: Manifest end
    end
end


Salsa.@querygroup MyQueryGroup begin
    Salsa.@input function manifest(db) :: Manifest end
    Salsa.@input function source_text(db, name::String) :: String end

    function ast(db, name::String) :: Ast end
    function whole_program_ast(db) :: Ast end
end

Salsa.@query function ast(db::MyQueryGroup, name::String)
    source = db.source_text[name]
    ast = Ast(source)
end

Salsa.@query function whole_program_ast(db::MyQueryGroup)
    result = Ast()
    for filename in db.manifest[]
        result = extend(result, ast(db, filename))
    end
    return result
end

@testset "Salsa example" begin
    db = MyQueryGroup()

    @test haskey(db.manifest) == false
    db.manifest[] = ["a.rs", "b.rs"]
    @test haskey(db.manifest) == true

    db.source_text["a.rs"] = "fn main() {}"
    db.source_text["b.rs"] = "fn bar();"
    @test haskey(db.source_text, "a.rs") == true
    @test haskey(db.source_text, "b.rs") == true
    @test haskey(db.source_text, "c.rs") == false

    @test db.__manifest_map[ManifestKey()].changed_at == 1
    @test db.__source_text_map[SourceTextKey("a.rs")].changed_at == 2
    @test db.__source_text_map[SourceTextKey("b.rs")].changed_at == 3

    whole_program_ast(db)

    @test db.__ast_map[AstKey("a.rs")].changed_at == 3
    @test db.__ast_map[AstKey("a.rs")].verified_at == 3
    @test db.__ast_map[AstKey("b.rs")].changed_at == 3
    @test db.__ast_map[AstKey("b.rs")].verified_at == 3

    @test db.__whole_program_ast_map[WholeProgramAstKey()].changed_at == 3
    @test db.__whole_program_ast_map[WholeProgramAstKey()].verified_at == 3

    db.source_text["a.rs"] = "fn foo() {}"

    @test db.__manifest_map[ManifestKey()].changed_at == 1
    @test db.__source_text_map[SourceTextKey("a.rs")].changed_at == 4
    @test db.__source_text_map[SourceTextKey("b.rs")].changed_at == 3

    whole_program_ast(db)

    @test db.__ast_map[AstKey("a.rs")].changed_at == 4
    @test db.__ast_map[AstKey("a.rs")].verified_at == 4
    @test db.__ast_map[AstKey("b.rs")].changed_at == 3
    @test db.__ast_map[AstKey("b.rs")].verified_at == 4

    @test db.__whole_program_ast_map[WholeProgramAstKey()].changed_at == 4
    @test db.__whole_program_ast_map[WholeProgramAstKey()].verified_at == 4

    # NOTE: users should not be calling `keys()` in derived queries, since it won't get added to dependency graph.
    filenames = sort([ name for name = keys(db.source_text) ])
    @test filenames == ["a.rs", "b.rs"]
end

# ----------------------------------

Salsa.@querygroup EarlyExitOptimizationTest begin
    Salsa.@input function input(db) :: Int end
    function ispositive(db, x::Int) :: Bool end
    function has_positive_input(db) :: Bool end
    function match_input_sign(db, v::Int) :: Int end
end
const ftrace = Set([])
Salsa.@query function ispositive(db::EarlyExitOptimizationTest, x::Int) :: Bool
    push!(ftrace, ispositive)
    x > 0
end
Salsa.@query function has_positive_input(db::EarlyExitOptimizationTest) :: Bool
    push!(ftrace, has_positive_input)
    ispositive(db, db.input[])
end
Salsa.@query function match_input_sign(db::EarlyExitOptimizationTest, v::Int) :: Int
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

Salsa.@querygroup ScalarValues begin
    Salsa.@input function sv(db) :: Int end
    Salsa.@input function arrayval(db) :: Vector{Int} end
end

@testset "scalar values in QueryGroup" begin
    @testset "default scalar values?" begin
        # TODO: *is* this broken? Maybe it's intended behavior (i.e. no default values)
        @test_broken ScalarValues().sv[] == 0
        @test_broken ScalarValues().arrayval[] == []
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

Salsa.@querygroup ErrorHandling begin
    Salsa.@input function v(db) :: Int end
    function square_root(db) :: Float64 end
end
Salsa.@query function square_root(db::ErrorHandling)
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
    @test_broken square_root(db) == 1  # ERROR: "Cycle in active query"
end

# ----------------------------------
Salsa.@querygroup MapAggregatesDB begin
    Salsa.@input function map(db, k::Int) :: Int end

    function numelts(db) :: Int end
    function mapkeys(db) :: Vector{Int} end
    function mapvals(db) :: Vector{Int} end
    function valsum(db) :: Int end
    function keysum(db) :: Int end

    # This actually loops over the elements, ∴ _touches_ them.
    function looped_valsum(db) :: Int end
end

Salsa.@query function numelts(db::MapAggregatesDB)
    length(db.map)
end

Salsa.@query mapvals(db::MapAggregatesDB) = sort(collect(values(db.map)))
Salsa.@query mapkeys(db::MapAggregatesDB) = sort(collect(keys(db.map)))
Salsa.@query function valsum(db::MapAggregatesDB)
    sum(values(db.map))
end
Salsa.@query function keysum(db::MapAggregatesDB)
    sum(keys(db.map))
end
Salsa.@query function looped_valsum(db::MapAggregatesDB)
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
        # TODO Collect fails on return value of keys():
        # Cannot `convert` an object of type Int64 to an object of type Main.SalsaTest.MapKey
        @test_broken sort(collect(keys(db.map))) == [1]

        # Verify updates
        db.map[2] = 20
        @test_broken sort(collect(keys(db.map))) == [1,2]
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
    @test_broken mapkeys(db) == []  # NOTE: keys is broken in general
    @test_broken keysum(db) == 0  # NOTE: keys is broken in general
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
@test_broken @eval begin
    Salsa.@querygroup MultiMethodFunctions begin
        function f(db) :: Any end
        function f(db, arg::Any) :: Any end
        function f(db, arg::Int) :: Int end
    end
    f(db) = 1
    f(db, arg::Any) = arg
    f(db, arg::Int) = arg
end

# ----------------------------------

Salsa.@querygroup AbstractTypes begin
    # Store abstract types in Input
    Salsa.@input function in_scalar(db) :: Any end
    Salsa.@input function in_parametric(db) :: Vector end  # matches any kind of Vector
    Salsa.@input function in_vec_of_any(db) :: Vector{Any} end  # only matches Vector{Any}
    Salsa.@input function in_int_vector(db) :: Vector{Int} end  # Should be able to initialize with []

    # Compute abstract types in derived  queries
    function derived_scalar(db) :: Any end
    function derived_parametric(db) :: Vector end  # matches any kind of Vector
    function derived_vec_of_any(db) :: Vector{Any} end  # Can be constructed from any returned Vector

    # Accept abstract params
    function derived_scalar_arg(db, arg::Any) :: Any end
    function derived_parametric_arg(db, arg::Vector) :: Vector end
    function derived_vec_of_any_arg(db, arg::Vector{Any}) :: Vector{Any} end
end

Salsa.@query derived_scalar(db::AbstractTypes) = 1
Salsa.@query derived_parametric(db::AbstractTypes) = [1,2,3]
Salsa.@query derived_vec_of_any(db::AbstractTypes) = [1,2,3]  # This will throw an Error

Salsa.@query derived_scalar_arg(db::AbstractTypes, arg::Any) = arg
Salsa.@query derived_parametric_arg(db::AbstractTypes, arg::Vector) = arg
Salsa.@query derived_vec_of_any_arg(db::AbstractTypes, arg::Vector{Any}) = arg

@testset "Returning AbstractTypes from QueryGroup" begin
    @testset "Assigning subtypes to Abstract Inputs" begin
        @test_broken AbstractTypes().in_scalar[] = 1  # Assignment of subtype fails
        @test_broken AbstractTypes().in_parametric[] = [1,2,3]

        # We should _probably_ be able to construct from a vector of Ints, since normal structs can:
        # ``` module M struct S x::Vector{Any} end end;   M.S([1,2])  # succeeds ```
        @test_broken AbstractTypes().in_vec_of_any[] = [1,2,3]
        @test (AbstractTypes().in_vec_of_any[] = []; true)

        # Should be able to construct a Vector{Int} with `[]`
        @test_broken AbstractTypes().in_int_vector[] = []
    end

    @testset "Returning subtypes from derived functions" begin
        # NOTE that you cannot infer the results of these functions, because the map holds
        # an abstract type for the return value.
        @test_broken derived_scalar(AbstractTypes()) == 1  # Returning subtype
        @test_broken derived_parametric(AbstractTypes()) == [1,2,3]  # Returning subtype

        @test_broken derived_vec_of_any(AbstractTypes()) == Any[1,2,3]  # Return value is `convert`able to Vecotr{Any}
    end

    @testset "Functions accept subtypes of Argument specifiers" begin
        @test_broken derived_scalar_arg(AbstractTypes(), 1) == 1  # Pass subtype
        @test_broken derived_parametric_arg(AbstractTypes(), [1,2,3]) == [1,2,3]  # Pass subtype

        @test_throws MethodError derived_vec_of_any_arg(AbstractTypes(), [1,2,3])  # Can't pass Vector{Int} to Vector{Any}
        @test derived_vec_of_any_arg(AbstractTypes(), Any[1,2,3]) == Any[1,2,3]  # Return value is `convert`able to Vecotr{Any}
    end
end

# ------------------------------------------

@testset "bad input" begin
    # This shouldn't parse, but it does, because apparently `struct S Vector{Int} end` is
    # valid Julia, and it just ignores the weird type in there....
    # We should probably catch this because it's easy to accidentally write this (i've
    # done it already).
    @test_broken begin
        # TODO: This should throw an exception: @test_throws Exception
        @eval Salsa.@querygroup Test begin
            Salsa.@input function arraymap(db, Vector{Int}) :: Int end
        end
        # Evalutate to false to represent the "brokenness", because can't compose
        # `@test_broken` and `@test_throws`.
        false
    end
end
@testset "weird inputs" begin
    # But we _should_ probably be able to accept this one; why does the user need to name
    # the keys in the input function?
    @test_broken @eval begin
        Salsa.@querygroup Test begin
            Salsa.@input function arraymap(db, ::Vector{Int}) :: Int end
        end
        true
    end
end

# -------------------------------------------

Salsa.@querygroup MutableKeysValues begin
    Salsa.@input function vec_to_int(db, k::Vector{Int}) :: Int end
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


# --------------------------------------------------
# End of tests, start of benchmark
# --------------------------------------------------

const bench_scale = 100000

@noinline function create_bench_db()
    db = MyQueryGroup()

    man = ["$i" for i in 1:bench_scale]
    set_manifest(db, man)
    for i in 1:bench_scale
        set_source_text(db, "$i", "program $i {}")
    end

    return db
end

@noinline function incr_bench_db()
    db = create_bench_db()
    whole_program_ast(db)
    set_source_text(db, "1", "program 1 { }")
    set_source_text(db, "$bench_scale", "program $bench_scale { }")

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

function profile_run()
    Profile.clear()
    Profile.init(n=10^7, delay=0.05)
    @profile for i in 1:10
        db = incr_bench_db()
        whole_program_ast(db)
    end
    statprofilehtml()
end

end
