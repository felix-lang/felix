import fbuild.builders.scala

def build(ctx):
    scala = fbuild.builders.scala.Builder(ctx)

    lib = scala.build_lib('lib.jar', ['world.scala'])

    ctx.logger.log(' * running script.scala:')
    scala.run_script('script.scala', classpaths=[lib])

    exe = scala.build_lib('exe.jar', ['compiled.scala'], classpaths=[lib])
    ctx.logger.log(' * running %s:' % exe)
    scala.run_class('HelloWorld', classpaths=[lib, exe])
