type direction = Fwd | Rev
val dir_rev : direction -> direction
type 'a dlst = { dir : direction; lst : 'a list; }
val dfwd : 'a dlst -> 'a list
val drev : 'a dlst -> 'a list
val dlst_fwd : 'a list -> 'a dlst
val dlst_rev : 'a list -> 'a dlst
val dlst_lst : 'a dlst -> 'a list
val dlst_dir : 'a dlst -> direction
val dlst_map : ('a -> 'b) -> 'a dlst -> 'b dlst
val append : 'a dlst -> 'a -> 'a dlst
val prepend : 'a dlst -> 'a -> 'a dlst
val cons : 'a list -> 'a -> 'a list
val concat : 'a dlst -> 'a dlst -> 'a dlst
