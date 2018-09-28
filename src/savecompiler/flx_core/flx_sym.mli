(** The symbol type. *)
type t = {
  id:Flx_id.t;
  sr:Flx_srcref.t;
  vs:Flx_types.ivs_list_t;
  pubmap:Flx_name_map.name_map_t;
  privmap:Flx_name_map.name_map_t;
  dirs:Flx_types.sdir_t list;
  symdef:Flx_types.symbol_definition_t;
}
