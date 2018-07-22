Objects
=======

Syntax
------

.. code-block:: felix

  satom := sadjectives "object" stvarlist slambda_fun_args fun_return_type "=" scompound 
  sfunction := "object" sdeclname sfun_arg* "implements" object_return_type "=" scompound
  sfunction := "object" sdeclname sfun_arg*  "=" scompound 
  sfunction :=
    "object" sdeclname sfun_arg* "extends" stypeexpr_comma_list
    "implements" object_return_type "=" scompound

  sfunction := "object" sdeclname sfun_arg*  "extends" stypeexpr_comma_list "=" scompound 
  sadjective := "method" =># "'Method";
  stmt := "interface" sdeclname stype_extension "{" srecord_type "}" =>#
    srecord_type := srecord_mem_decl (";" srecord_mem_decl)* ";"
    stypelist := stypeexpr ("," stypeexpr)* 
    stype_extension := "extends" stypelist 
    stype_extension := sepsilon


Basic Objects
-------------

Felix has a special kind of function which is used to construct
a Java like object:

.. code-block:: felix

  interface person_t {
    get_name: 1 -> string;
    get_age: 1 -> double;
  }

  object person (name:string, age: double) 
    implements person_t = 
  {
    method fun get_name => name;
    method fun get_age => age;
  }

  var joe = person ("joe", 42);
  println$ joe.get_name () + " is " + (joe.get_age ()) .str

An interface is precisely a record type with an alternate syntax.
An object is precisely a function returning a record consisting
of closures of all the functions and procedures marked as
methods. The optional implements clause specifies the return type.

Java like objects were implemented as a joke to show how powerful
Felix is .. however they turned out to be quite useful and are
used heavily in the representation of plugins.

The implements clause is optional.

 
