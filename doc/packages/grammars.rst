Package: src/packages/grammars.fdoc

============ ==================================
key          file                               
============ ==================================
grammars.flx share/lib/std/strings/grammars.flx 
============ ==================================

========
Grammars
========


Grammar
=======



.. index:: Grammars(class)
.. index:: str(fun)
.. code-block:: felix

  //[grammars.flx]
  
  class Grammars {
  
  typedef generic_gramentry_t[T] = string * T;
  typedef generic_gramlib_t[T] = list[generic_gramentry_t[T]];
  typedef generic_grammar_t[T] = string * generic_gramlib_t[T];
  
  fun generic_cls[T] 
    (generic_add: list[string] -> T -> list[string])
    (lib:generic_gramlib_t[T]) 
    (unprocessed: list[string]) 
    (processed:list[string])
  : list[string] 
  =>
    match unprocessed with
    | Empty => processed
    | Cons (h,tail) =>
      if h in processed then generic_cls generic_add lib tail processed else
      match find lib h with
      | Some p =>
        let unprocessed = generic_add tail p in
        generic_cls generic_add lib unprocessed (Cons (h,processed))
      | None => 
        fun_fail[list[string]] ("MISSING NONTERMINAL " + h)
      endmatch
    endmatch
  ;
  
  fun generic_closure[T] 
    (generic_add: list[string] -> T -> list[string])
    (g:generic_grammar_t[T]) 
  : list[string] =>
    match g with
    | start, lib => generic_cls generic_add lib ([start]) Empty[string]
  ;
  
  // NOTE: this depends on Recognisers, but Recognisers
  // depends on Grammars. BAD BAD.
  
  typedef open_prod_t[T] =
  ( 
    | `Terminal of string * Recognisers::recog_t
    | `Nonterminal of string
    | `Epsilon
    | `Seq of list[T]
    | `Alt of list[T]
  )
  ;
  
  typedef prod_t = open_prod_t[prod_t];
  
  instance[T with Str[T]] Str[open_prod_t[T]] 
  {
    fun str: open_prod_t[T] -> string =
    | `Terminal (s,r) => '"' + s + '"'
    | `Nonterminal name => name
    | `Epsilon => "Eps"
    | `Seq ss => "(" + catmap " " (str of T) ss + ")"
    | `Alt ss => "[" + catmap " | " (str of T) ss + "]"
    ;
  }
  
  typedef open_gramentry_t[T] = string * open_prod_t[T];
  typedef open_gramlib_t[T] = list[open_gramentry_t[T]];
  typedef open_grammar_t[T] = string * open_gramlib_t[T];
  
  
  typedef gramentry_t = open_gramentry_t[prod_t];
  typedef gramlib_t = open_gramlib_t[prod_t];
  typedef grammar_t = open_grammar_t[prod_t];
  
Grammar Operations
==================


Closure
-------


