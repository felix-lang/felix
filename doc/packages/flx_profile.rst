Package: src/packages/flx_profile.fdoc


==========================================
Flx tool config and profile establishment.
==========================================


=============== ===================================
key             file                                
=============== ===================================
flx_profile.flx share/lib/std/felix/flx_profile.flx 
config.flx      share/lib/std/felix/config.flx      
=============== ===================================


Profile
-------

The profile is the most basic low level configuration data,
which determines where to find everything.


.. index:: FlxProfile(class)
.. index:: dflt_profile(fun)
.. index:: def(type)
.. code-block:: felix

  //[flx_profile.flx]
  class FlxProfile
  {
    fun dflt_profile () = 
    {
      fun / (x:string, y:string) => Filename::join (x,y);
      var HOME= 
        let h = Env::getenv "HOME" in
          if h!="" then h 
          elif PLAT_WIN32 then Env::getenv "USERPROFILE"
          else ""
          endif
      ;
      if HOME == "" do
        eprintln$ "HOME (or USERPROFILE on WIN32) environment variable is not set.  Please set HOME before building."; 
        // this one is pretty fatal :-)
        System::exit 1;
      done 
  
     
      var FLX_HOME_DIR = Env::getenv("FLX_HOME_DIR",HOME/".felix");
      var FLX_CACHE_TOP = Env::getenv("FLX_CACHE_TOP",FLX_HOME_DIR/"cache");
      var FLX_PROFILE_DIR = Env::getenv("FLX_PROFILE_DIR",FLX_HOME_DIR/"config");
  
      var FLX_CACHE_DIR = Env::getenv("FLX_CACHE_DIR",FLX_CACHE_TOP / "binary");
      var FLX_OUTPUT_DIR = Env::getenv("FLX_OUTPUT_DIR",FLX_CACHE_TOP / "text");
      return 
        (
         FLX_HOME_DIR=FLX_HOME_DIR, 
         FLX_PROFILE_DIR=FLX_PROFILE_DIR, 
         FLX_CACHE_DIR=FLX_CACHE_DIR,
         FLX_OUTPUT_DIR=FLX_OUTPUT_DIR
        )
      ;
    }
  
    typedef profile_type = typeof (#dflt_profile);
    instance Str[profile_type] {
      fun str(x:profile_type) => 
         "FLX_HOME_DIR="+x.FLX_HOME_DIR+"\n"+
         "FLX_PROFILE_DIR="+x.FLX_PROFILE_DIR+"\n"+
         "FLX_CACHE_DIR="+x.FLX_CACHE_DIR+"\n"+
         "FLX_OUTPUT_DIR="+x.FLX_OUTPUT_DIR+"\n"
      ;
    }
  }
  


Config.
-------

A more detailed layout configuration based
on command line switches and the base profile.

.. index:: Config(class)
.. index:: def(type)
.. index:: set_libs_and_rtls(proc)
.. index:: cascade_FLX_INSTALL_DIR(proc)
.. index:: cascade_FLX_TARGET_DIR(proc)
.. index:: cascade_FLX_SHARE_DIR(proc)
.. index:: cascade_FLX_HOME_DIR(proc)
.. index:: copy_profile(proc)
.. index:: dflt_config(fun)
.. index:: process_config_text(proc)
.. index:: config_env_overrides(proc)
.. index:: process_config_text_with_env_overrides(proc)
.. index:: std_config(fun)
.. code-block:: felix

  //[config.flx]
  include "std/version";
  include "std/felix/flx_profile";
  
  
  
  class Config {
    typedef config_type = (
      FLX_INSTALL_DIR: string,
      FLX_SHARE_DIR: string,
      FLX_TARGET_DIR: string,
      FLX_HOME_DIR: string,
      FLX_PROFILE_DIR: string,
      FLX_CACHE_DIR: string,
      FLX_OUTPUT_DIR: string,
      FLX_CONFIG_DIRS: list[string],
      FLX_LIB_DIRS: list[string],
      FLX_RTL_DIRS: list[string]
    );
  
    instance Str[config_type] {
      fun str (x:config_type) : string =
      {
        var s = "";
        reserve$ &s,1000;
        s+="(FLX_INSTALL_DIR="+ x.FLX_INSTALL_DIR+",\n";
        s+="FLX_SHARE_DIR="+ x.FLX_SHARE_DIR+",\n";
        s+="FLX_TARGET_DIR="+ x.FLX_TARGET_DIR+",\n";
        s+="FLX_HOME_DIR="+ x.FLX_HOME_DIR+",\n";
        s+="FLX_PROFILE_DIR="+ x.FLX_PROFILE_DIR+",\n";
        s+="FLX_CACHE_DIR="+ x.FLX_CACHE_DIR+",\n";
        s+="FLX_OUTPUT_DIR="+ x.FLX_OUTPUT_DIR+",\n";
        s+="FLX_LIB_DIRS="+ x.FLX_LIB_DIRS.str+",\n";
        s+="FLX_CONFIG_DIRS="+ x.FLX_CONFIG_DIRS.str+",\n";
        s+="FLX_RTL_DIRS="+ x.FLX_RTL_DIRS.str+")\n";
        return s;
      }
    }
  
    private fun / (x:string, y:string) => Filename::join (x,y);
      
    proc set_libs_and_rtls (x: &config_type)
    {
      x.FLX_LIB_DIRS <- list (x*.FLX_SHARE_DIR/"lib", x*.FLX_TARGET_DIR/"lib");
      x.FLX_RTL_DIRS <- list (x*.FLX_SHARE_DIR/"lib"/"rtl", x*.FLX_TARGET_DIR/"lib"/"rtl");
    }
  
    proc cascade_FLX_INSTALL_DIR (x: &config_type)  (y: string) = {
      x.FLX_INSTALL_DIR <- y;
      cascade_FLX_TARGET_DIR x (y/"host");
      cascade_FLX_SHARE_DIR x (y/"share");
    }
  
    proc cascade_FLX_TARGET_DIR (x: &config_type)  (y: string) = {
      x.FLX_TARGET_DIR <- y;
      x.FLX_CONFIG_DIRS <- list[string] (y/"config");
      set_libs_and_rtls x;
    }
  
    proc cascade_FLX_SHARE_DIR (x: &config_type)  (y: string) = {
      x.FLX_SHARE_DIR <- y;
      set_libs_and_rtls x;
    }
  
    proc cascade_FLX_HOME_DIR (x: &config_type)  (y: string) = {
      x.FLX_HOME_DIR <- y;
      x.FLX_PROFILE_DIR <- y/"config";
      x.FLX_CACHE_DIR <- y/"cache"/"binary";
      x.FLX_OUTPUT_DIR <- y/"cache"/"text";
    }
  
    proc copy_profile (cfg: &config_type) (profile: FlxProfile::profile_type)
    {
      cfg.FLX_HOME_DIR <- profile.FLX_HOME_DIR;
      cfg.FLX_PROFILE_DIR <- profile.FLX_PROFILE_DIR;
      cfg.FLX_CACHE_DIR <- profile.FLX_CACHE_DIR;
      cfg.FLX_OUTPUT_DIR <- profile.FLX_OUTPUT_DIR;
    }
  
    fun dflt_config() :config_type = {
      var profile = FlxProfile::dflt_profile();
      var cfg : config_type;
      copy_profile &cfg profile;
  
      // global defaults
      var PREFIX = Filename::root_subdir "usr"/"local"/"lib";
  
      var INSTALL_ROOT_TOPDIR= PREFIX/"felix";
      var INSTALL_ROOT = INSTALL_ROOT_TOPDIR/ ("felix-"+Version::felix_version);
      cascade_FLX_INSTALL_DIR &cfg INSTALL_ROOT;
      return cfg;
    }
  
    proc process_config_text (cfg:&config_type) (text:string)
    {
  
      var re = RE2 ("([-a-zA-Z_]+) *: *(.*)");
      var FLX_INSTALL_DIR = cfg*.FLX_INSTALL_DIR;
  
      var lines = split (text, char "\n");
      for line in lines do
        var found = Match (re, line);
        match found with
        | Some v when v.len.int == 3 => 
          var p = v.1;
          var a = strip v.2;
          match p with
          | "FLX_INSTALL_DIR" => 
            FLX_INSTALL_DIR = a;
  //println$ "processing config text, setting FLX_INSTALL_DIR=" + a;
            cascade_FLX_INSTALL_DIR cfg a; 
  
          | "FLX_TARGET_SUBDIR" => 
            if FLX_INSTALL_DIR != "" do
              cascade_FLX_TARGET_DIR cfg (FLX_INSTALL_DIR / a);
            else
              eprintln$ "Cannot set FLX_TARGET_SUBDIR without setting FLX_INSTALL_DIR";
              // this one is pretty fatal :-)
              System::exit 1;
            done
  
          | "FLX_SHARE_DIR" => cascade_FLX_SHARE_DIR cfg a; 
          | "FLX_TARGET_DIR" => cascade_FLX_TARGET_DIR cfg a; 
          | "FLX_HOME_DIR" => cascade_FLX_HOME_DIR cfg a; 
          | "FLX_PROFILE_DIR" => cfg.FLX_PROFILE_DIR <- a; 
          | "FLX_CONFIG_DIRS" => cfg.FLX_CONFIG_DIRS <- respectful_split a; 
          | "FLX_CACHE_DIR" => cfg.FLX_CACHE_DIR <- a; 
          | "FLX_OUTPUT_DIR" => cfg.FLX_OUTPUT_DIR <- a; 
          | "FLX_LIB_DIRS" => cfg.FLX_LIB_DIRS <-  respectful_split a; 
          | "FLX_RTL_DIRS" => cfg.FLX_RTL_DIRS <- respectful_split a; 
          | _ => ;
          endmatch;
        | #None => ;
        endmatch;
      done
    }
  
  
    proc config_env_overrides (cfg:&config_type) 
    {
  
      match Env::getenv ("FLX_INSTALL_DIR","") with
      | "" => ;
      | x => 
  //println$ "ENVIRONMENT OVERRIDE FOR FLX_INSTALL_DIR=" + x;
        cascade_FLX_INSTALL_DIR cfg x;
      endmatch;
  
      match Env::getenv ("FLX_SHARE_DIR","") with
      | "" => ;
      | x => cascade_FLX_SHARE_DIR cfg x;
      endmatch;
  
      match Env::getenv ("FLX_TARGET_DIR","") with
      | "" => ;
      | x => cascade_FLX_TARGET_DIR cfg x;
      endmatch;
  
      match Env::getenv ("FLX_CONFIG_DIRS","") with
      | "" => ;
      | x => cfg.FLX_CONFIG_DIRS <- respectful_split x;
      endmatch;
  
      match Env::getenv ("FLX_LIB_DIRS","") with
      | "" => ;
      | x => cfg.FLX_LIB_DIRS <- respectful_split x;
      endmatch;
  
      match Env::getenv ("FLX_RTL_DIRS","") with
      | "" => ;
      | x => cfg.FLX_RTL_DIRS <- respectful_split x;
      endmatch;
    }
  
    proc process_config_text_with_env_overrides (cfg:&config_type) (text:string)
    {
      process_config_text cfg text;
      config_env_overrides cfg;
    }
  
    fun std_config () = {
  //println$ "Setting up default config";
      var cfg = #dflt_config; 
  //println$ "Processing config file felix.fpc with env overrides";
      process_config_text_with_env_overrides &cfg (load (cfg.FLX_PROFILE_DIR / "felix.fpc"));
      return cfg; 
    }
  
  }
  



