#line 62 "interscript/src/frames.ipk"
class install_frame:
  def __init__(self):
    py = 'python'+self.python_version
    self.platform_independent_install_point = sys.prefix
    self.python_header_install_point = sys.prefix + '/include/'+py
    self.python_module_install_point = sys.prefix + '/lib/'+py
    self.python_platform_install_point = sys.prefix + '/lib/'+py+'/plat-'+platform.python_plat

    self.platform_dependent_install_point = sys.exec_prefix
    self.python_config_install_point = sys.exec_prefix + '/lib/'+self.python_version+'/config'
    self.python_dynload_install_point = sys.exec_prefix + '/lib/'+self.python_version+'/lib-dynload'

    # this is where stuff goes during development
    d = self.development_point = {}
    d['documentation']=self.platform.get_working_directory()
    d['python module']=self.platform.get_working_directory()
    d['python package']=self.platform.get_working_directory()
    d['python cmodule']=self.platform.get_working_directory()
    d['python script']=self.platform.get_working_directory()
    d['executable']=self.platform.get_working_directory()

    # this is where standard stuff gets installed for general use
    d = self.standard_install_point = {}
    d['documentation']=sys.prefix+'/doc'
    d['python module']=self.python_module_install_point
    d['python package']=self.python_module_install_point
    d['python cmodule']=self.python_dynload_install_point
    d['python script']=sys.exec_prefix+'/bin'
    d['executable']=sys.exec_prefix+'/bin'

    # this is where platform dependent stuff gets installed for general use
    d = self.platform_install_point = {}
    d['documentation']=sys.prefix+'/doc'
    d['python module']=self.python_module_install_point
    d['python package']=self.python_module_install_point
    d['python cmodule']=self.python_dynload_install_point
    d['python script']=sys.exec_prefix+'/bin'
    d['executable']=sys.exec_prefix+'/bin'

    # this is where site dependent stuff gets installed for general use
    d = self.site_install_point = {}
    d['documentation']=sys.prefix+'/doc'
    d['python module']=self.python_module_install_point+'/site-packages'
    d['python package']=self.python_module_install_point+'/site-packages'
    d['python cmodule']=self.python_dynload_install_point+'/site-packages'
    d['python script']=sys.exec_prefix+'/bin'
    d['executable']=sys.exec_prefix+'/bin'

    # this is where user-group dependent stuff gets installed for use
    d = self.usergrp_install_point = {}
    d['documentation']=sys.prefix+'/doc'
    d['python module']=self.python_module_install_point+'/site-packages'
    d['python package']=self.python_module_install_point+'/site-packages'
    d['python cmodule']=self.python_dynload_install_point+'/site-packages'
    d['python script']=sys.exec_prefix+'/bin'
    d['executable']=sys.exec_prefix+'/bin'

    # this is where user dependent stuff gets installed for use
    d = self.user_install_point = {}
    d['documentation']=sys.prefix+'/doc'
    d['python module']=self.python_module_install_point+'/site-packages'
    d['python package']=self.python_module_install_point+'/site-packages'
    d['python cmodule']=self.python_dynload_install_point+'/site-packages'
    d['python script']=sys.exec_prefix+'/bin'
    d['executable']=sys.exec_prefix+'/bin'

    self.install_point = {
      'dev': self.development_point,
      'std': self.standard_install_point,
      'plat': self.platform_install_point,
      'site': self.site_install_point,
      'grp': self.usergrp_install_point,
      'user': self.user_install_point
      }

    self.trust_map = {
      'experimental':'dev',
      'test':'dev',
      'alpha':'dev',
      'beta':'user',
      'production':'user'
      }

  def print_install(self):
    print('INSTALL POINTS')
    print('----------------------------')
    for k in list(self.install_point.keys()):
      print('Install Point',k)
      d = self.install_point[k]
      for t in list(d.keys()):
        print(' ',t,'-->',d[t])
      print('----------------------------')
      print()
    print('TRUST MAP')
    print('----------------------------')
    for k in list(self.trust_map.keys()):
      print(' ',k,'-->',self.trust_map[k])
    print('----------------------------')
    print()


