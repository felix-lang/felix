try:
  from config.target_pari_config import *
except ImportError:
  import config
  from fbuild.flxbuild.config_support import cwrite, pa
  HAVE_PARI = config.TARGET_CXX.check_header_exists('pari.h')
  f = cwrite('target_pari')
  pa(f, locals(), "HAVE_PARI")
  f.close()
