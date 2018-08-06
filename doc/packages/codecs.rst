Package: src/packages/codecs.fdoc


======
Codecs
======

============= =================================
key           file                              
============= =================================
__init__.flx  share/lib/std/codec/__init__.flx  
base64.flx    share/lib/std/codec/base64.flx    
csv.flx       share/lib/std/codec/csv.flx       
uri_codec.flx share/lib/std/codec/uri_codec.flx 
============= =================================

Synopsis
========



.. code-block:: felix
  //[__init__.flx]
  
  include "std/codec/csv";
  include "std/codec/base64";
  include "std/codec/uri_codec";
  
  
Base64 
=======



.. index:: Base64
.. code-block:: felix
  //[base64.flx]
  
  //$ Base64 encode/decode functions.
  //$ http://en.wikipedia.org/wiki/Base64
  
  class Base64 {
    val b64_chars = ('A','B','C','D','E','F','G','H','I','J','K','L','M',
                     'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
                     'a','b','c','d','e','f','g','h','i','j','k','l','m',
                     'n','o','p','q','r','s','t','u','v','w','x','y','z',
                     '0','1','2','3','4','5','6','7','8','9','+','/');
  
    val b64_string = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  
    gen make_string: size*char->string = "::std::string ($1, $2)";
  
    instance Bits[char] {
      fun \& : char * char -> char = "$1&$2";
      fun \| : char * char -> char = "$1|$2";
    }
  
    open Bits[char];
    private fun >> : char * int -> char = "$1>>$2";
    private fun << : char * int -> char = "$1<<$2";
    private fun utiny_of: char -> utiny = "(unsigned char)$1:cast" is cast;
  
    // Encode function derived from encode function 
    // http://www.source-code.biz/base64coder/java/Base64Coder.java.txt 
    // by Christian d'Heureuse, Inventec Informatik AG, Zurich, Switzerland
    //$ Returns base 64 encoding of supplied string inp.
    fun encode (inp:string) => encode(inp,0,inp.len.int);
  
    fun encode (inp:string, iOff:int, iLen:int) : string = {
      val oDataLen = (iLen*4+2)/3;       // output length without padding
      val oLen = ((iLen+2)/3)*4;         // output length including padding
      // if using darray would use this
      //var out = darray[char]( size oLen,char(0));
      var out:string = "";//make_string(size oLen,char(0));
      var ip = iOff;
      var iEnd = iOff + iLen;
      var op = 0;
      while (ip < iEnd) do
        val i0 = inp.[ip] \& char(0xff);ip++;
        val i1 = if ip < iEnd then inp.[ip] \& char(0xff) else char(0) endif;if ip < iEnd do ip++; done
        val i2 = if ip < iEnd then inp.[ip] \& char(0xff) else char(0) endif;if ip < iEnd do ip++; done
        val o0 = i0 >> 2;
        val o1 = ((i0 \&   char(3)) << 4) \| (i1 >> 4);
        val o2 = ((i1 \& char(0xf)) << 2) \| (i2 >> 6);
        val o3 = i2 \& char(0x3F);
        out  += char (b64_chars.(utiny_of(o0)));op++;
        out  += char (b64_chars.(utiny_of(o1)));op++;
        out  += if op < oDataLen then char (b64_chars.(utiny_of(o2))) else char('=') endif;
        // if usaing darray then would use this
        //out.[op] = char (b64_chars.[utiny_of(o0)]);op++;
        //out.[op] = char (b64_chars.[utiny_of(o1)]);op++;
        //out.[op] = if op < oDataLen then char (b64_chars.[utiny_of(o2)]) else char('=') endif;
        op++;
          out += if op < oDataLen then  char(b64_chars.(utiny_of(o3))) else char('=') endif;
          //if using darray would do this
          //out.[op] = if op < oDataLen then  char(b64_chars.[utiny_of(o3)]) else char('=') endif;
        op++; 
     done
     return out; 
    }
  
  
    //$ Wraps encoded string after ll chars, no newline on last line.
    fun wrap (b64_str:string,ll:uint) : string = {
      var ret = "";
      val n = b64_str.len.uint;
      val whole = n/ll;
      val rmd = n%ll;
      reserve (&ret, n+whole+1u);
      for var i in 0ui upto whole - 2u do
        ret += b64_str.[i*ll to (i + 1u)*ll]+"\n";
      done
      ret += b64_str.[(whole - 1u)*ll to (whole)*ll];
      if rmd > 0u do
        ret+= "\n" + b64_str.[whole*ll to whole*ll+rmd];
      done
      return ret;
    }
  
    //$ Decodes supplied base 64 encoded string.
    fun decode(enc_str:string) = {
      var in_len:uint = enc_str.len.uint;
      var i:int = 0;
      var j:int = 0;
      var in_ = 0;
      var char_array_4:char^4;
      var char_array_3:char^3;
      var ret:string;
  
      while in_len > 0ui and ( enc_str.[in_] != char('=')) do 
        //(and is_base64(enc_str[in_])) 
        in_len--;
        &char_array_4.i <- enc_str.[in_]; i++; in_++;
        if (i == 4) do
          for var ip in  0 upto 3 do
            set(&char_array_4,ip, ( match find(b64_string,char_array_4.(ip)) with 
              |Some v => char(v)
              |_ => char(0)
            endmatch));
          done
          set(&char_array_3,0,(char_array_4.(0) << 2) \| ((char_array_4.(1) \& char(0x30)) >> 4));
          set(&char_array_3,1,((char_array_4.(1) \& char(0xf)) << 4) \| ((char_array_4.(2) \& char(0x3c)) >> 2));
          set(&char_array_3,2,((char_array_4.(2) \& char(0x3)) << 6) \| char_array_4.(3));
          for var l in  0 upto 2 do
            ret = ret + char_array_3.(l);
          done
          i = 0;
        done
      done
    if (i > 0 ) do
      set(&char_array_4,i, char_array_3.(1)); 
      for var m in i upto 3 do
        set(&char_array_4,i, char(0));
      done
      for var k in 0 upto 3 do
        set(&char_array_4,k,( match find(b64_string,char_array_4.(k)) with 
              |Some v => char(v)
              |_ => char(0)
            endmatch));
      done
      set(&char_array_3,0, (char_array_4.(0) << 2) \| ((char_array_4.(1) \& char(0x30)) >> 4));
      set(&char_array_3,1, ((char_array_4.(1) \& char(0xf)) << 4) \| ((char_array_4.(2) \& char(0x3c)) >> 2));
      set(&char_array_3,2, ((char_array_4.(2) \& char(0x3)) << 6) \| char_array_4.(3));
  
      for var n in  0 upto  (i - 2) do
         ret += char_array_3.(n);
      done
    done
    return ret;
    }
  
  }
  
Csv 
====



.. index:: Csv
.. code-block:: felix
  //[csv.flx]
  
  //$ Comma Separated Values (CSV) reader
  //$ Splits a string like 1,2,"hell" up into three strings.
  class Csv {
    open List;
  
    //$ Fetch a value string res from position i of string s.
    //$ Update i past the comma ready to fetch another value.
    proc get_csv_value(s:string, i:&int,res:&string) {
      var r = "";
      proc add(j:int) { r += s.[j]; }
      n := s.len.int;
      enum state_t = skip,collect,quote;
      fun eq(a:state_t, b:state_t)=> caseno a == caseno b;
  
      var state = skip;
      ech:for var j in *i upto n - 1 do
        ch := s.[j];
        if ch == char "," do 
          match state with 
          | #quote => add j;
          | _ => break ech;
          endmatch;
        elif ch == char " " do 
          match state with
          | #skip => continue ech;
          | #quote => add j;
          | #collect => state = skip;
          endmatch;
        elif ch == char '"' do 
          match state with
          | #quote => state = skip;
          | _ => state = quote;
          endmatch;
        else 
          add j;
        done;
      done;
      i <- j+1;
      res <- r;
    }
  
    //$ Fetch all the values in a CSV string
    //$ and return them as list.
    fun get_csv_values(s:string): list[string] = {
      var v: list[string] = Empty[string];
      var res = "";
      var pos = 0;
      n := s.len.int;
      while pos < n do
        get_csv_value (s, &pos, &res);
        if res.len.int >0 do v += res; done;
      done;
      return v;
    }
  }
  
URI Codec
=========


