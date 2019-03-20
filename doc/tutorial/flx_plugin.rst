Flx Plugins
===========

The Felix command line tool consists of a program `dflx` which is loads several plugins,
primarily the compiler drivers.

There is a version `flx` which consists of exactly the same program,
but with the plugins pre-linked. Here is the prelink code:

.. code-block:: felix

  class FlxPluginSymbols 
  {

    // We have to do this dummy requirements because static
    // linking removes
    requires package "re2";
    requires package "faio";
    requires package "flx_arun";

    open Dynlink;

    // Now add all the symbols.
    proc addsymbols ()
    {
      static-link-plugin 
        toolchain_clang_macosx,
        toolchain_iphoneos,
        toolchain_iphonesimulator,
        toolchain_clang_linux,
        toolchain_gcc_macosx,
        toolchain_gcc_linux,
        toolchain_msvc_win
      ;
      // flx
      static-link-symbol dflx_create_thread_frame in plugin dflx;
      static-link-symbol dflx_flx_start in plugin dflx;
      
    }
  }

  // Add the symbols
  FlxPluginSymbols::addsymbols;

  // Now invoke the program!
  val linstance =  Dynlink::prepare_lib("dflx");
  var init: cont = Dynlink::get_init linstance;

  Fibres::chain init;


  In this case, the program runs after initialisation, 
so we get the mainline continuation `init` from the plugin instance `linstance`
and chain to it.



