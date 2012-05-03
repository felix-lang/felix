import fbuild
import fbuild.builders.ghc

def build(ctx):
    ghc = fbuild.builders.ghc.Builder(ctx)

    exe = ghc.build_exe('greet', ['Main.hs'])
    ctx.execute([exe, 'world!'])
