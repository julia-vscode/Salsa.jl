module SalsaTest

using Salsa
using Salsa: AbstractComponent, InputMap, InputScalar, @component, @input, @connect, SalsaDerivedException
using BenchmarkTools
using Profile
using Test

using Statistics: mean


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
    # NOTE: @connect is the simplest way to embed another component and make sure they have
    # the same Runtime. It's important that they share the same runtime.
    Salsa.@connect compiler::Compiler
end
Salsa.@component TestStorage begin
    Salsa.@connect workspace::Workspace
end
@testset "connected components" begin
    w = Workspace()
    w.i2[] = 1
    w.i2[] = 2
    @test w.i2[] == 2
    @test w.compiler.runtime.current_revision == w.runtime.current_revision == 2

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
        sum([c.compiler.i[0] + x , y...])
    end
    c.workspace.compiler.i[0] = 1
    foofunc(c.workspace, 1, [1,2])  # runs foofunc (and prints)
    foofunc(c.workspace, 1, [1,2])  # returns cached value
    foofunc(c.workspace, 3, [1,2])  # runs again
    @test count == 2

    # Multi-level derived functions
    Salsa.@derived function outer(c::TestStorage)::Int
        foofunc(c.workspace, 2, Int[])
    end
    count = 0
    outer(c)  # Increments count
    outer(c)  # This result should be cached
    @test count == 1
    c.workspace.compiler.i[0] = 2
    outer(c)  # Increments count
    outer(c)  # This result should be cached
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
@test alldefs(w) == ""
w.relnames[] = [:A]
w.defs[:A] = "1"
@test alldefs(w) == "1"


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

    _ast_map = Salsa.get_map_for_key(db.runtime,
        Salsa.DerivedKey{typeof(ast),Tuple{MyQueryGroup,String}}())
    @test _ast_map[(db, "a.rs",)].changed_at == 3
    @test _ast_map[(db, "a.rs",)].verified_at == 3
    @test _ast_map[(db, "b.rs",)].changed_at == 3
    @test _ast_map[(db, "b.rs",)].verified_at == 3

    _whole_program_ast_map = Salsa.get_map_for_key(db.runtime,
        Salsa.DerivedKey{typeof(whole_program_ast),Tuple{MyQueryGroup}}())
    @test _whole_program_ast_map[(db,)].changed_at == 3
    @test _whole_program_ast_map[(db,)].verified_at == 3

    db.source_text["a.rs"] = "fn foo() {}"

    @test db.manifest.v[] isa Some
    @test db.manifest.v[].value.changed_at == 1
    @test db.source_text.v["a.rs"].changed_at == 4
    @test db.source_text.v["b.rs"].changed_at == 3

    whole_program_ast(db)

    @test _ast_map[(db,"a.rs",)].changed_at == 4
    @test _ast_map[(db,"a.rs",)].verified_at == 4
    @test _ast_map[(db,"b.rs",)].changed_at == 3
    @test _ast_map[(db,"b.rs",)].verified_at == 4

    @test _whole_program_ast_map[(db,)].changed_at == 4
    @test _whole_program_ast_map[(db,)].verified_at == 4

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
    Salsa.@input v :: InputScalar{Int}
    Salsa.@input m :: InputMap{Int,Int}
end
Salsa.@derived function square_root(st::ErrorHandling)
    sqrt(st.v[])
end
Salsa.@derived function get_val(st::ErrorHandling, key)
    st.m[key]
end
Salsa.@derived function val_times_sqrt(st::ErrorHandling, key)
    get_val(st, key) * square_root(st)
end

@testset "Robust to derived functions that throw errors" begin
    db = ErrorHandling()

    # Setting a value that should work as expected
    db.v[] = 1
    @test square_root(db) == 1

    # Setting a value that will cause square_root() to throw an Exception
    db.v[] = -1
    @test_throws DomainError square_root(db)

    # Now test that it's recovered gracefully from the error, and we can still use the DB
    db.v[] = 1
    @test square_root(db) == 1
end

@testset "Multi-level derived functions that throw errors #1180" begin
    db = ErrorHandling()

    # Cause `square_root()` to throw an exeception, when being called from within another
    # derived function.
    db.v[] = -1
    db.m[1] = 2
    # Attempts 2 * sqrt(-1), and throws an error
    @test_throws DomainError val_times_sqrt(db, 1)

    # Now test that it's recovered gracefully from the error, and we can still use the DB
    db.v[] = 1
    @test square_root(db) == 1
    @test val_times_sqrt(db, 1) == 2  # 2 * sqrt(1)

    # Now check that we also recover from KeyErrors when reading from a map:
    # Throw error:
    @test_throws KeyError val_times_sqrt(db, 100)  # No key 100
    # But this call still works:
    @test val_times_sqrt(db, 1) == 2  # 2 * sqrt(1)
end
# -----------------------------------------------

# Check KEY DELETIONS

Salsa.@component ClassGrades begin
    @input all_student_ids :: InputScalar{Tuple{Vararg{Int}}}  # Using a Tuple b/c it's immutable
    @input student_grades :: InputMap{Int, Int}
end
@derived function average_grade(state)
    tot = sum(state.student_grades[id] for id in state.all_student_ids[])
    tot / length(state.all_student_ids[])
end

@testset "Key Deletions" begin
    state = ClassGrades()
    state.all_student_ids[] = (1,2)
    state.student_grades[1] = 4.0
    state.student_grades[2] = 2.0
    @test average_grade(state) == 3

    # Delete student `1`
    delete!(state.student_grades, 1)
    state.all_student_ids[] = (2,)
    @test average_grade(state) == 2.0

    # <Test that Salsa correctly throws an error given a programming bug>
    # Delete student only from student_grades but leave in all_student_ids (a programming error)
    delete!(state.student_grades, 2)
    @test_throws KeyError average_grade(state)
end

# ----------------------------------
Salsa.@component MapAggregatesDB begin
    Salsa.@input map::Salsa.InputMap{Int, Int}
end

# Should throw an error for reflection inside a Derived function!
Salsa.@derived function numelts(db::AbstractComponent)
    length(db.map)
end

# Should throw an error for reflection inside a Derived function!
Salsa.@derived mapvals(db::AbstractComponent) = sort(collect(values(db.map)))
Salsa.@derived mapkeys(db::AbstractComponent) = sort(collect(keys(db.map)))
Salsa.@derived function valsum(db::AbstractComponent)
    sum(values(db.map))
end
Salsa.@derived function keysum(db::AbstractComponent)
    sum(keys(db.map))
end
# This actually loops over the elements, ∴ _touches_ them (also an error).
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

    # From within a derived function, calling anything but getindex fails:

    # Reflection functions throw an error on an empty map.
    @test_throws ErrorException numelts(db)
    @test_throws ErrorException mapvals(db)
    @test_throws ErrorException valsum(db)
    @test_throws ErrorException mapkeys(db)
    @test_throws ErrorException keysum(db)
    # The version that loops over the (k,v) items, and therefore actually touches them.
    @test_throws ErrorException looped_valsum(db)

    # If you update the values, calling anything but getindex still fails.
    db.map[1] = 10
    db.map[2] = 20
    @test_throws ErrorException numelts(db)
    @test_throws ErrorException mapvals(db)
    @test_throws ErrorException valsum(db)
    @test_throws ErrorException mapkeys(db)
    @test_throws ErrorException keysum(db)
    @test_throws ErrorException looped_valsum(db)
end

# ----------------------------------

# Allow functions with multiple methods
# NOTE: Also testing varied arg syntax (`x`, `x::T`, `::T`).
Salsa.@derived multi_method(db::AbstractComponent) = 1
Salsa.@derived multi_method(db::AbstractComponent, arg) = arg
Salsa.@derived multi_method(db::AbstractComponent, arg::Int) = 10
Salsa.@derived multi_method(db::AbstractComponent, ::Symbol) = "!"

Salsa.@component MultiMethodFunctions begin
end
@testset "multiple methods" begin
    @test multi_method(MultiMethodFunctions()) == 1
    @test multi_method(MultiMethodFunctions(), 2) == 10
    @test multi_method(MultiMethodFunctions(), "hi") == "hi"
    @test multi_method(MultiMethodFunctions(), :bang) == "!"
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
    # now it can't be used as a key, because it doesn't match the original hash (i think)?
    @test_throws KeyError db.vec_to_int[a]
    @test_throws KeyError db.vec_to_int[[1,2,3]] == 1
end

# --- Manifest insertions and deletions ----------------------

Salsa.@component ManifestAndMap begin
    Salsa.@input all_names :: Salsa.InputScalar{Tuple{Vararg{String}}}
    Salsa.@input grades :: Salsa.InputMap{String,Int}  # 0 - 100

    Salsa.@input all_pass_fails :: Salsa.InputScalar{Tuple{Vararg{String}}}
end

# Firewall pattern for all_pass_fails
@derived function is_pass_fail(st, name::String) :: Bool
    name in st.all_pass_fails[]
end
@derived function grade_for_student(st, name::String) :: Int
    if is_pass_fail(st, name)
        st.grades[name] > 50 ? 100 : 0
    else
        st.grades[name]
    end
end
@derived function course_average(st) :: Union{Int, Nothing}
    if isempty(st.all_names[])
        nothing
    else
        Int(round(mean([grade_for_student(st,name) for name in st.all_names[]])))
    end
end

initial_grades = Dict("John"    =>  100,
                      "Sam"     =>  66,
                      "Amy"     =>  99,  # pass/fail -> 100
                      "Nathan"  =>  30,  # pass/fail ->   0
                     )

@testset "Manifests and Maps" begin
    st = ManifestAndMap()
    st.all_names[] = tuple(keys(initial_grades)...)

    for (n,g) in initial_grades
        st.grades[n] = g
    end

    st.all_pass_fails[] = ("Nathan", "Amy")

    # mean([100, 66, 100, 0]) == 66
    @test course_average(st) == 66

    @testset "insertions" begin
        st.all_names[] = tuple(keys(initial_grades)..., "Vikram")

        st.grades["Vikram"] = 33

        # mean([100, 66, 100, 0, 33]) == 60
        @test course_average(st) == 60

        # Now make Vikram pass/fail
        st.all_pass_fails[] = ("Nathan", "Amy", "Vikram")
        # mean([100, 66, 100, 0, 0]) == 60
        @test course_average(st) == 53
    end

    @testset "deletions" begin
        st.all_names[] = tuple(keys(initial_grades)...,)

        delete!(st.grades, "Vikram")
        st.all_pass_fails[] = ("Nathan", "Amy")

        # mean([100, 66, 100, 0]) == 66
        @test course_average(st) == 66

        # It's okay to have names in here that aren't used
        st.all_pass_fails[] = ("Nathan", "Amy", "Vikram")

        # mean([100, 66, 100, 0]) == 66
        @test course_average(st) == 66
    end
    @testset "empty!(::InputMap)" begin
        empty!(st.grades)

        # Attempting to compute an average with an inconsistent manifest and map will throw
        # a KeyError:
        @test_throws KeyError course_average(st)

        st.all_names[] = ()
        @test isequal(course_average(st), nothing)  # Returns missing since nothing to average
    end
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
using Test
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

@derived function x_at(a::A, k)
    a.x[k]
end
@derived function x_at(b::B, k)
    b.a.x[k]
end

@testset "composed component" begin
    b = B()
    b.a.x[1] = 10
    @test b.a.x[1] == 10
    @test x_at(b, 1) == 10
    @test x_at(b.a, 1) == 10
    b.a.x[1] = 20
    @test b.a.x[1] == 20
    @test x_at(b, 1) == 20
    @test x_at(b.a, 1) == 20
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
