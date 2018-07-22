Integer Literals
================

Syntax
------

.. code-block:: felix


  SCHEME """
  (define (findradix s)  ; find the radix of integer lexeme
    (let*
      (
        (n (string-length s))
        (result
          (cond
            ((prefix? "0b" s)`(,(substring s 2 n) 2))
            ((prefix? "0o" s)`(,(substring s 2 n) 8))
            ((prefix? "0d" s)`(,(substring s 2 n) 10))
            ((prefix? "0x" s)`(,(substring s 2 n) 16))
            (else `(,s 10))
          )
        )
      )
      result
    )
  )
  """;

  SCHEME """
  (define (findtype s) ;; find type of integer lexeme
    (let*
      (
        (n (string-length s))
        (result
          (cond
            ((suffix? "ut" s)`(,(substring s 0 (- n 2)) "utiny"))
            ((suffix? "tu" s)`(,(substring s 0 (- n 2)) "utiny"))
            ((suffix? "t" s)`(,(substring s 0 (- n 1)) "tiny"))

            ((suffix? "us" s)`(,(substring s 0 (- n 2)) "ushort"))
            ((suffix? "su" s)`(,(substring s 0 (- n 2)) "ushort"))
            ((suffix? "s" s)`(,(substring s 0 (- n 1)) "short"))

            ((suffix? "ui" s)`(,(substring s 0 (- n 2)) "uint"))
            ((suffix? "iu" s)`(,(substring s 0 (- n 2)) "uint"))
            ((suffix? "i" s)`(,(substring s 0 (- n 1)) "int"))

            ((suffix? "uz" s)`(,(substring s 0 (- n 2)) "size"))
            ((suffix? "zu" s)`(,(substring s 0 (- n 2)) "size"))
            ((suffix? "z" s)`(,(substring s 0 (- n 1)) "ssize"))

            ((suffix? "uj" s)`(,(substring s 0 (- n 2)) "uintmax"))
            ((suffix? "ju" s)`(,(substring s 0 (- n 2)) "uintmax"))
            ((suffix? "j" s)`(,(substring s 0 (- n 1)) "intmax"))

            ((suffix? "up" s)`(,(substring s 0 (- n 2)) "uintptr"))
            ((suffix? "pu" s)`(,(substring s 0 (- n 2)) "uintptr"))
            ((suffix? "p" s)`(,(substring s 0 (- n 1)) "intptr"))

            ((suffix? "ud" s)`(,(substring s 0 (- n 2)) "uptrdiff"))
            ((suffix? "du" s)`(,(substring s 0 (- n 2)) "uptrdiff"))
            ((suffix? "d" s)`(,(substring s 0 (- n 1)) "ptrdiff"))

            ;; must come first!
            ((suffix? "uvl" s)`(,(substring s 0 (- n 3)) "uvlong"))
            ((suffix? "vlu" s)`(,(substring s 0 (- n 3)) "uvlong"))
            ((suffix? "ulv" s)`(,(substring s 0 (- n 3)) "uvlong"))
            ((suffix? "lvu" s)`(,(substring s 0 (- n 3)) "uvlong"))
            ((suffix? "llu" s)`(,(substring s 0 (- n 3)) "uvlong"))
            ((suffix? "ull" s)`(,(substring s 0 (- n 3)) "uvlong"))

            ((suffix? "uv" s)`(,(substring s 0 (- n 2)) "uvlong"))
            ((suffix? "vu" s)`(,(substring s 0 (- n 2)) "uvlong"))

            ((suffix? "lv" s)`(,(substring s 0 (- n 2)) "vlong"))
            ((suffix? "vl" s)`(,(substring s 0 (- n 2)) "vlong"))
            ((suffix? "ll" s)`(,(substring s 0 (- n 2)) "vlong"))

            ;; comes next
            ((suffix? "ul" s)`(,(substring s 0 (- n 2)) "ulong"))
            ((suffix? "lu" s)`(,(substring s 0 (- n 2)) "ulong"))

            ;; last
            ((suffix? "v" s)`(,(substring s 0 (- n 1)) "vlong"))
            ((suffix? "u" s)`(,(substring s 0 (- n 1)) "uint"))
            ((suffix? "l" s)`(,(substring s 0 (- n 1)) "long"))

            ;; exact
            ((suffix? "u8" s)`(,(substring s 0 (- n 2)) "uint8"))
            ((suffix? "u16" s)`(,(substring s 0 (- n 3)) "uint16"))
            ((suffix? "u32" s)`(,(substring s 0 (- n 3)) "uint32"))
            ((suffix? "u64" s)`(,(substring s 0 (- n 3)) "uint64"))
            ((suffix? "i8" s)`(,(substring s 0 (- n 2)) "int8"))
            ((suffix? "i16" s)`(,(substring s 0 (- n 3)) "int16"))
            ((suffix? "i32" s)`(,(substring s 0 (- n 3)) "int32"))
            ((suffix? "i64" s)`(,(substring s 0 (- n 3)) "int64"))
            (else `(,s "int"))
          )
        )
      )
      result
    )
  )
  """;

  SCHEME """
  (define (parse-int s)
    (let*
      (
        (s (tolower-string s))
        (x (findradix s))
        (radix (second x))
        (x (first x))
        (x (findtype x))
        (type (second x))
        (digits (first x))
        (value (string->number digits radix))
      )
      (if (equal? value #f)
         (begin
           (newline)
           (display "Invalid integer literal ") (display s)
           (newline)
           (display "Radix ")(display radix)
           (newline)
           (display "Type ")(display type)
           (newline)
           (display "Digits ")(display digits)
           (newline)
           error
         )
         `(,type ,value)
      )
    )
  )
  """;

  //$ Integer literals.
  //$
  //$ Felix integer literals consist of an optional radix specifer,
  //$ a sequence of digits of the radix type, possibly separated
  //$ by an underscore (_) character, and a trailing type specifier.
  //$
  //$ The radix can be:
  //$ 0b, 0B - binary
  //$ 0o, 0O - octal
  //$ 0d, 0D - decimal
  //$ 0x, 0X - hex
  //$
  //$ The default is decimal.
  //$ NOTE: unlike C a leading 0 in does NOT denote octal.
  //$
  //$ Underscores are allowed between digits or the radix
  //$ and the first digit, or between the digits and type specifier.
  //$
  //$ The adaptable signed type specifiers are:
  //$
  //$ t        -- tiny   (char as int)
  //$ s        -- short
  //$ i        -- int
  //$ l        -- long
  //$ v,ll     -- vlong (long long in C)
  //$ z        -- ssize (ssize_t in C, a signed variant of size_t)
  //$ j        -- intmax
  //$ p        -- intptr
  //$ d        -- ptrdiff
  //$
  //$ These may be upper of lower case.
  //$ A "u" or "U" before or after such specifier indicates
  //$ the correspondin unsigned type.
  //$
  //$ The follingw exact type specifiers can be given:
  //$
  //$      "i8" | "i16" | "i32" | "i64"
  //$    | "u8" | "u16" | "u32" | "u64"
  //$    | "I8" | "I16" | "I32" | "I64"
  //$    | "U8" | "U16" | "U32" | "U64";
  //$
  //$ The default type is "int".
  //$

  syntax felix_int_lexer {
    /* integers */
    regdef bin_lit  = '0' ('b' | 'B') (underscore ? bindigit) +;
    regdef oct_lit  = '0' ('o' | 'O') (underscore ? octdigit) +;
    regdef dec_lit  = '0' ('d' | 'D') (underscore ? digit) +;
    regdef dflt_dec_lit  =  digit (underscore ? digit) *;
    regdef hex_lit  = '0' ('x' | 'X') (underscore ? hexdigit)  +;
    regdef int_prefix = bin_lit | oct_lit | dec_lit | dflt_dec_lit | hex_lit;

    regdef fastint_type_suffix =
      't'|'T'|'s'|'S'|'i'|'I'|'l'|'L'|'v'|'V'|"ll"|"LL"|"z"|"Z"|"j"|"J"|"p"|"P"|"d"|"D";
    regdef exactint_type_suffix =
        "i8" | "i16" | "i32" | "i64"
      | "u8" | "u16" | "u32" | "u64"
      | "I8" | "I16" | "I32" | "I64"
      | "U8" | "U16" | "U32" | "U64";

    regdef signind = 'u' | 'U';

    regdef int_type_suffix =
        '_'? exactint_type_suffix
      | ('_'? fastint_type_suffix)? ('_'? signind)?
      | ('_'? signind)? ('_'? fastint_type_suffix)?;

    regdef int_lit = int_prefix int_type_suffix;

    // Untyped integer literals.
    literal int_prefix =># """
    (let*
      (
        (val (stripus _1))
        (x (parse-int val))
        (type (first x))
        (value (second x))
      )
      value
    )
    """;
    sinteger := int_prefix =># "_1";

    // Typed integer literal.
    literal int_lit =># """
    (let*
      (
        (val (stripus _1))
        (x (parse-int val))
        (type (first x))
        (value (second x))
        (fvalue (number->string value))
        (cvalue fvalue)       ;; FIXME!!
      )
      `(,type ,fvalue ,cvalue)
    )
    """;
    sliteral := int_lit =># "`(ast_literal ,_sr ,@_1)";

    // Typed signed integer constant.
    sintegral := int_lit =># "_1";
    sintegral := "-" int_lit =># """
    (let*
      (
        (type (first _2))
        (val (second _2))
        (val (* -1 val))
      )
      `(,type ,val)
    )
    """;

    strint := sintegral =># "(second _1)";
  }



