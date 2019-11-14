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
end

function extend!(ast::Ast, piece::Ast)
    ast
end

function extend!(ast::Ast, source::String)
    ast
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
    ast = Ast()
    extend!(ast, source)
end

Salsa.@query function whole_program_ast(db::MyQueryGroup)
    result = Ast()
    for filename in db.manifest[]
        extend!(result, ast(db, filename))
    end
    return result
end

@testset "Salsa" begin
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

    db.source_text["a.rs"] = "fn main() { }"

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
end

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
