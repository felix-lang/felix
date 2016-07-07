
let debug = try ignore(Sys.getenv "FLX_DEBUG_PARSER"); true with Not_found -> false

let fresh_dssl = {
  Flx_token.macros = [];
  Flx_token.regexps = [];
  Flx_token.prios = [];
  Flx_token.rules = [];
  Flx_token.deps = [];
  Flx_token.privacy = Flx_drules.Drules.empty;
}

let global_data = {
  Flx_token.pcounter = ref 1;
  Flx_token.env = Flx_ocs_init.init_env ();
  Flx_token.pdebug = ref debug;
  Flx_token.parsing_device = ref None;
}

let local_data = {
  Flx_token.global_regexps = [];
  Flx_token.drules = Flx_drules.Drules.empty;
  Flx_token.installed_dssls = [];
  Flx_token.scm = [];
  Flx_token.rev_stmts_as_scheme = [];
}


