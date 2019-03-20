Flx Server Plugins
==================

The Felix webserver consists of a program `dflx_web` which is loads several plugins.
There is a version `flx_web` which consists of exactly the same program,
but with the plugins pre-linked. Here is the prelink code:

.. code-block:: felix

  class WebserverPluginSymbols 
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
        fdoc2html,
        flx2html,
        fpc2html,
        py2html,
        ocaml2html,
        cpp2html,
        fdoc_scanner,
        fdoc_slideshow,
        fdoc_heading,
        fdoc_fileseq,
        fdoc_paragraph,
        fdoc_button,
        fdoc_frame,
        fdoc_edit,
        toc_menu
      ;
      // webserver
      static-link-symbol dflx_web_create_thread_frame in plugin dflx_web;
      static-link-symbol dflx_web_flx_start in plugin dflx_web;
      
    }
  }

  // Add the symbols
  WebserverPluginSymbols::addsymbols;

  // Now invoke the webserver!
  println$ "Running webserver";
  val linstance =  Dynlink::prepare_lib("dflx_web");
  println$ "Webserver prepared";
  var init: cont = Dynlink::get_init linstance;

  Fibres::chain init;


In this case, the program runs continuously after initialisation, 
so we get the mainline continuation `init` from the plugin instance `linstance`
and chain to it.



