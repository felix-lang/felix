import fbuild.builders.java

def build(ctx):
    java = fbuild.builders.java.Builder(ctx)

    lib = java.build_lib('lib.jar', ['World.java'])
    exe = java.build_lib('exe.jar', ['HelloWorld.java'], classpaths=[lib])
    ctx.logger.log(' * running %s:' % exe)
    java.run_class('HelloWorld', classpaths=[lib, exe])
