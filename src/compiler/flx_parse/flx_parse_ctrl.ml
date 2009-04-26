open Flx_ast
open Flx_token
open Flx_exceptions
open Flx_parse

let parse_file
  (filename : string)
  (basedir :string)
  (include_dirs : string list)
  cache_dir
  expand_expr
  auto_imports
=
    Flx_parse.parse_file 
    filename basedir include_dirs cache_dir 
    auto_imports
