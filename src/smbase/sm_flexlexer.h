// sm_flexlexer.h
// a layer of indirection to try to work with different
// installed versions of Flex


// Basically, what has happened is the last version of flex created by
// Vern Paxson et al. is 2.5.4, and a lot of distributions use that.
// More recently some other people started a project hosted at
// Sourceforge, and their current (only?) release is flex-2.5.31.
// Unfortunately, the authors of 2.5.31 do not appear concerned with
// maintaining compatibility for C++ scanners.
//
// See bug "[ 751550 ] c++ derived scanners will not compile",
// http://sourceforge.net/tracker/index.php?func=detail&aid=751550&group_id=72099&atid=533377
//
// So in an attempt to make Elkhound/Elsa work, this file is where
// I'll put my hacks.


// workaround for flex-2.5.31 bug
#ifdef yyFlexLexer
  #undef yyFlexLexer
#endif

// now get the installed FlexLexer.h.. this nominally lives in
// /usr/include or /usr/local/include, depending on how Flex is
// installed
//#include <FlexLexer.h>


// for now I keep the rest but I think I may be able to delete
// it soon..
//#if 0     // terminates at EOF
// FELIX uses this text, never the system FlexLexer.h
#if 1


// copy of /usr/include/FlexLexer.h from flex-2.5.4


// To use this file, use the same flex version as listed above.


// Why is this file distributed with smbase?
//
// 1. I want my distribution tarballs to include the *output* of
// tools like flex/bison, but flex's output has an #include of
// FlexLexer.h.  Since I want the distribution to be self-contained,
// that means distributing the .h file as well.
//
// 2. It avoids version mismatch.  Some people have flex-2.5.31,
// some have 2.5.4.  Some of my classes depend on the internals of
// the FlexLexer class which have changed between the versions, and
// making it compatible with both is difficult.


// What is its copyright status?
//
// I've put most of smbase into the public domain.  However, the
// Regents retain copyright on this file.  I am in effect just
// providing a convenient distribution mechanism for smbase users
// (users *could* just download it themselves, for free, just like
// smbase).


// ------------------ original file below -----------------
// $Header$

// FlexLexer.h -- define interfaces for lexical analyzer classes generated
//                by flex

// Copyright (c) 1993 The Regents of the University of California.
// All rights reserved.
//
// This code is derived from software contributed to Berkeley by
// Kent Williams and Tom Epperly.
//
// Redistribution and use in source and binary forms with or without
// modification are permitted provided that: (1) source distributions retain
// this entire copyright notice and comment, and (2) distributions including
// binaries display the following acknowledgement:  ``This product includes
// software developed by the University of California, Berkeley and its
// contributors'' in the documentation or other materials provided with the
// distribution and in all advertising materials mentioning features or use
// of this software.  Neither the name of the University nor the names of
// its contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.

// THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

// This file defines FlexLexer, an abstract class which specifies the
// external interface provided to flex C++ lexer objects, and yyFlexLexer,
// which defines a particular lexer class.
//
// If you want to create multiple lexer classes, you use the -P flag
// to rename each yyFlexLexer to some other xxFlexLexer.  You then
// include <FlexLexer.h> in your other sources once per lexer class:
//
//      #undef yyFlexLexer
//      #define yyFlexLexer xxFlexLexer
//      #include <FlexLexer.h>
//
//      #undef yyFlexLexer
//      #define yyFlexLexer zzFlexLexer
//      #include <FlexLexer.h>
//      ...

#ifndef __FLEX_LEXER_H
// Never included before - need to define base class.
#define __FLEX_LEXER_H
#include <iostream>

// FELIX: always C++ compiles anyhow
//extern "C++" {

struct yy_buffer_state;
typedef int yy_state_type;

class FlexLexer {
public:
        virtual ~FlexLexer()    { }

        const char* YYText()    { return yytext; }
        int YYLeng()            { return yyleng; }

        virtual void
                yy_switch_to_buffer( struct yy_buffer_state* new_buffer ) = 0;
        virtual struct yy_buffer_state*
                yy_create_buffer( std::istream* s, int size ) = 0;
        virtual void yy_delete_buffer( struct yy_buffer_state* b ) = 0;
        virtual void yyrestart( std::istream* s ) = 0;

        virtual int yylex() = 0;

        // Call yylex with new input/output sources.
        int yylex( std::istream* new_in, std::ostream* new_out = 0 )
                {
                switch_streams( new_in, new_out );
                return yylex();
                }

        // Switch to new input/output streams.  A nil stream pointer
        // indicates "keep the current one".
        virtual void switch_streams( std::istream* new_in = 0,
                                        std::ostream* new_out = 0 ) = 0;

        int lineno() const              { return yylineno; }

        int debug() const               { return yy_flex_debug; }
        void set_debug( int flag )      { yy_flex_debug = flag; }

protected:
        char* yytext;
        int yyleng;
        int yylineno;           // only maintained if you use %option yylineno
        int yy_flex_debug;      // only has effect with -d or "%option debug"
};

//FELIX: always C++
//}

#endif

#if defined(yyFlexLexer) || ! defined(yyFlexLexerOnce)
// Either this is the first time through (yyFlexLexerOnce not defined),
// or this is a repeated include to define a different flavor of
// yyFlexLexer, as discussed in the flex man page.
#define yyFlexLexerOnce

class yyFlexLexer : public FlexLexer {
public:
        // arg_yyin and arg_yyout default to the cin and cout, but we
        // only make that assignment when initializing in yylex().
        yyFlexLexer( std::istream* arg_yyin = 0, std::ostream* arg_yyout = 0 );

        virtual ~yyFlexLexer();

        void yy_switch_to_buffer( struct yy_buffer_state* new_buffer );
        struct yy_buffer_state* yy_create_buffer( std::istream* s, int size );
        void yy_delete_buffer( struct yy_buffer_state* b );
        void yyrestart( std::istream* s );

        virtual int yylex();
        virtual void switch_streams( std::istream* new_in, std::ostream* new_out );

protected:
        virtual int LexerInput( char* buf, int max_size );
        virtual void LexerOutput( const char* buf, int size );
        virtual void LexerError( const char* msg );

        void yyunput( int c, char* buf_ptr );
        int yyinput();

        void yy_load_buffer_state();
        void yy_init_buffer( struct yy_buffer_state* b, std::istream* s );
        void yy_flush_buffer( struct yy_buffer_state* b );

        int yy_start_stack_ptr;
        int yy_start_stack_depth;
        int* yy_start_stack;

        void yy_push_state( int new_state );
        void yy_pop_state();
        int yy_top_state();

        yy_state_type yy_get_previous_state();
        yy_state_type yy_try_NUL_trans( yy_state_type current_state );
        int yy_get_next_buffer();

        std::istream* yyin;  // input source for default LexerInput
        std::ostream* yyout; // output sink for default LexerOutput

        struct yy_buffer_state* yy_current_buffer;

        // yy_hold_char holds the character lost when yytext is formed.
        char yy_hold_char;

        // Number of characters read into yy_ch_buf.
        int yy_n_chars;

        // Points to current character in buffer.
        char* yy_c_buf_p;

        int yy_init;            // whether we need to initialize
        int yy_start;           // start state number

        // Flag which is used to allow yywrap()'s to do buffer switches
        // instead of setting up a fresh yyin.  A bit of a hack ...
        int yy_did_buffer_switch_on_eof;

        // The following are not always needed, but may be depending
        // on use of certain flex features (like REJECT or yymore()).

        yy_state_type yy_last_accepting_state;
        char* yy_last_accepting_cpos;

        yy_state_type* yy_state_buf;
        yy_state_type* yy_state_ptr;

        char* yy_full_match;
        int* yy_full_state;
        int yy_full_lp;

        int yy_lp;
        int yy_looking_for_trail_begin;

        int yy_more_flag;
        int yy_more_len;
        int yy_more_offset;
        int yy_prev_more_offset;
};

#endif

#endif // 0, from top of file
