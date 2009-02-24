try:
  from config.target_gsl_config import *
except ImportError:
  import config
  from fbuild.flxbuild.config_support import cwrite, pa
  HAVE_GSL = config.TARGET_CXX.check_header_exists('gsl/gsl_blas.h')
  f = cwrite('target_gsl')
  pa(f, locals(), "HAVE_GSL")
  f.close()
