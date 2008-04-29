open List

type direction = Fwd | Rev

let dir_rev = function | Fwd -> Rev | Rev->Fwd

type 'a dlst = { dir:direction; lst: 'a list }

let dfwd = function
  | {dir=Fwd; lst=l} -> l
  | {dir=Rev; lst=l} -> rev l

let drev = function
  | {dir=Fwd; lst=l} -> l
  | {dir=Rev; lst=l} -> rev l

let dlst_fwd = function l -> {dir=Fwd; lst=l}
let dlst_rev = function l -> {dir=Rev; lst=l}

let dlst_lst = function {lst=l} -> l
let dlst_dir = function {dir=d} -> d

let dlst_map f {dir=d; lst=l} = {dir=dir_rev d; lst=rev_map f l}

let append = function
  | {dir=Fwd; lst=l} -> (fun e -> {dir=Rev; lst=e::rev l})
  | {dir=Rev; lst=l} as d -> (fun e -> {d with lst=e::l})

let prepend = function
  | {dir=Rev; lst=l} -> (function e -> {dir=Fwd; lst=e::rev l})
  | {dir=Fwd; lst=l} as d -> (function e -> {d with lst=e::l})

let cons = function lst -> (function elt -> elt :: lst)

let concat a b = match (a,b) with
  | {dir=Rev; lst=l1}, {dir=Fwd; lst=l2} ->
    {dir=Rev; lst=fold_left cons l1 l2}

  | {dir=Fwd; lst=l1}, {dir=Fwd; lst=l2} ->
    {dir=Rev; lst=fold_left cons (rev l1) (rev l2)}

  | {dir=Rev; lst=l1}, {dir=Rev; lst=l2} ->
    {dir=Rev; lst=fold_left cons l1 (rev l2)}

  | {dir=Fwd; lst=l1}, {dir=Rev; lst=l2} ->
    {dir=Rev; lst=fold_left cons (rev l1) l2}
