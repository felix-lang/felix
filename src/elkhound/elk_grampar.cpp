// grampar.cc            see license.txt for copyright and terms of use
// additional C++ code for the grammar parser; in essence,
// build the grammar internal representation out of what
// the user supplies in a .gr file

#include "elk_grampar.h"
#include "ast_gramlex.h"
#include "sm_trace.h"
#include "elk_gramast.ast.gen.h"
#include "elk_grammar.h"
#include "sm_owner.h"
#include "sm_syserr.h"
#include "sm_strutil.h"
#include "elk_grampar.tab.h"
#include "sm_array.h"
#include "elk_mlsstr.h"

#include <fstream>         // std::ifstream
#include <ctype.h>           // isspace, isalnum

#define LIT_STR(s) LocString(SL_INIT, grammarStringTable.add(s))


// ------------------------- Environment ------------------------
Environment::Environment(Grammar &G)
  : g(G),
    prevEnv(NULL),
    nontermDecls(),
    errorCount(0),
    errors(errorCount)
{}

Environment::Environment(Environment &prev)
  : g(prev.g),
    prevEnv(&prev),
    nontermDecls(prev.nontermDecls),
    errorCount(-1000),      // should never be used
    errors(prev.errors)     // copy parent's 'errors' reference
{}

Environment::~Environment()
{}


// -------------------- XASTParse --------------------
STATICDEF sm_string XASTParse::
  constructMsg(LocString const &tok, char const *msg)
{
  if (tok.validLoc()) {
    return sm_stringc << tok.locString() << ": near " << tok
                   << ", " << msg;
  }
  else {
    return sm_string(msg);
  }
}

XASTParse::XASTParse(LocString const &tok, char const *m)
  : xBase(constructMsg(tok, m)),
    failToken(tok),
    message(m)
{}


XASTParse::XASTParse(XASTParse const &obj)
  : xBase(obj),
    DMEMB(failToken),
    DMEMB(message)
{}

XASTParse::~XASTParse()
{}


// -------------------- AST parser support ---------------------
// fwd-decl of parsing fns
void astParseGrammar(Grammar &g, GrammarAST *treeTop);
void astParseTerminals(Environment &env, TF_terminals const &terms);
void astParseDDM(Environment &env, Symbol *sym,
                 ASTList<SpecFunc> const &funcs);
void astParseNonterm(Environment &env, TF_nonterm const *nt);
void astParseProduction(Environment &env, Nonterminal *nonterm,
                        ProdDecl const *prod);


// really a static semantic error, more than a parse error..
void astParseError(LocString const &failToken, char const *msg)
{
  THROW(XASTParse(failToken, msg));
}

void astParseError(char const *msg)
{
  LocString ls;   // no location info
  THROW(XASTParse(ls, msg));
}

// print the same message, but keep going anyway
void astParseErrorCont(Environment &env, LocString const &failToken,
                       char const *msg)
{
  XASTParse x(failToken, msg);
  std::cout << x.why() << std::endl;
  env.errors++;
}


// to put as the catch block; so far it's kind of ad-hoc where
// I actually put 'try' blocks..
#define CATCH_APPLY_CONTEXT(tok)        \
  catch (XASTParse &x) {                \
    /* leave unchanged */               \
    throw x;                            \
  }                                     \
  catch (xBase &x) {                    \
    /* add context */                   \
    astParseError(tok, x.why());        \
    throw 0;     /* silence warning */  \
  }


// ---------------------- AST "parser" --------------------------
// set the annotation pointers
void setAnnotations(GrammarAST *ast)
{
  // work through the toplevel forms
  FOREACH_ASTLIST_NC(TopForm, ast->forms, iter) {
    ASTSWITCH(TopForm, iter.data()) {
      ASTCASE(TF_terminals, t) {
        if (!ast->terms) {
          ast->terms = t;
        }
        else {
          astParseError("there is more than one 'Terminals' section");
        }
      }

      ASTNEXT(TF_nonterm, nt) {
        if (!ast->firstNT) {
          ast->firstNT = nt;
        }
      }

      ASTENDCASED
    }
  }

  if (!ast->terms) {
    astParseError("'Terminals' specification is missing");
  }
  if (!ast->firstNT) {
    astParseError("you have to have at least one nonterminal");
  }
}


LocString extractActionClassName(LocString const &body)
{
  // find start of first token
  char const *start = body.str;
  while (isspace(*start)) start++;

  // find end of first token
  char const *p = start;
  while (isspace(*p)) p++;
  while (isalnum(*p) || *p=='_') p++;

  // yield that, with the same source location
  return LocString(body.loc, grammarStringTable.add(sm_string(start, p-start)));
}


// handle TF_verbatim and TF_option
void astParseOptions(Grammar &g, GrammarAST *ast)
{
  // handle TF_verbatim and TF_option
  FOREACH_ASTLIST_NC(TopForm, ast->forms, iter) {
    ASTSWITCH(TopForm, iter.data()) {
      ASTCASE(TF_context, c) {
        // overwrite the context class name, and append to
        // its body verbatim list
        g.actionClassName = extractActionClassName(c->body);

        // 11/13/04: There is a subtle problem with keeping the body
        // from the base specification, when the following conditions
        // hold:
        //   - the base spec is compiled on its own (w/o the extension)
        //   - some translation unit "A" sees the resulting .gr.gen.h file
        //   - the extension spec is compiled
        //   - some translation unit "B" sees the resulting .gr.gen.h file
        //   - A and B are linked together in one executable
        // In that case, the context_class from the base will have an
        // inconsistent definition in A and B, since in A it will be
        // whatever the user wrote plus, the declarations for the
        // action functions, whereas in B it will be just what the
        // user wrote, since the action functions end up in the
        // extension context_class.
        //
        // What is even more subtle is the *manifestation* of this
        // problem, which is linking problems with vtables.  C++
        // compilers do not explicitly check that classes declared in
        // multiple translation units have identical declarations
        // (token for token), but they *do* of course rely on them
        // being so.  That reliance shows up in the decisions
        // regarding which module has the vtable, among other places.
        // So this problem did not show up immediately, and was only
        // revealed as initially mysterious portability problems
        // (since my development toolchain happend to be fairly
        // lenient w.r.t. vtable placement).
        //
        // Therefore the new policy is that context_classes from the
        // base are *not* emitted, and consequently it is impossible
        // to inherit from them in subsequent context_classes.  The
        // user must put data/functions that are meant to be shared
        // into a common base class that is *not* the context_class
        // of any grammar or extension.
        //
        // old:
        //g.actionClasses.append(new LocString(c->body));
        //
        // new:
        g.actionClasses.deleteAll();
        g.actionClasses.append(new LocString(c->body));
      }

      ASTNEXT(TF_verbatim, v) {
        if (v->isImpl) {
          g.implVerbatim.append(new LocString(v->code));
        }
        else {
          g.verbatim.append(new LocString(v->code));
        }
      }

      ASTNEXT(TF_option, op) {
        LocString const &name = op->name;
        int value = op->value;
        bool boolVal = !!value;

        if (name.equals("useGCDefaults")) {
          g.useGCDefaults = boolVal;
        }
        else if (name.equals("defaultMergeAborts")) {
          g.defaultMergeAborts = boolVal;
        }
        else if (name.equals("shift_reduce_conflicts")) {
          g.expectedSR = value;
        }
        else if (name.equals("reduce_reduce_conflicts")) {
          g.expectedRR = value;
        }
        else if (name.equals("unreachable_nonterminals")) {
          g.expectedUNRNonterms = value;
        }
        else if (name.equals("unreachable_terminals")) {
          g.expectedUNRTerms = value;
        }
        else if (name.equals("lang_OCaml")) {
          //g.targetLang = "OCaml";
          //
          // I'm retarded.. I need to know if we're parsing ocaml *before*
          // we actually parse it, otherwise I can't skip the embedded
          // action fragments properly!
          astParseError(name, "The `lang_OCaml' option has been replaced with "
                              "the `-ocaml' command-line switch.  Please use "
                              "that instead.  (Sorry for the inconvenience.)");
        }
        else {
          astParseError(name, "unknown option name");
        }
      }

      ASTENDCASED
    }
  }
}


// map the grammar definition AST into a Grammar data structure
void astParseGrammar(Grammar &g, GrammarAST *ast)
{
  // default, empty environment
  Environment env(g);

  // handle TF_terminals
  astParseTerminals(env, *(ast->terms));

  // process all nonterminal declarations first, so while we're
  // looking at their bodies we can tell if one isn't declared
  {
    FOREACH_ASTLIST(TopForm, ast->forms, iter) {
      if (!iter.data()->isTF_nonterm()) continue;
      TF_nonterm const *nt = iter.data()->asTF_nontermC();

      // check for already declared
      if (env.nontermDecls.isMapped(nt->name)) {
        astParseError(nt->name, "nonterminal already declared");
      }

      // make the Grammar object to represent the new nonterminal
      env.g.getOrMakeNonterminal(nt->name);

      // add this decl to our running list (in the original environment)
      env.nontermDecls.add(nt->name, const_cast<TF_nonterm*>(nt));
    }
  }

  // process nonterminal bodies
  {
    FOREACH_ASTLIST(TopForm, ast->forms, iter) {
      if (!iter.data()->isTF_nonterm()) continue;
      TF_nonterm const *nt = iter.data()->asTF_nontermC();

      // new environment since it can contain a grouping construct
      // (at this very moment it actually can't because there is no syntax..)
      Environment newEnv(env);

      // parse it
      astParseNonterm(newEnv, nt);
    }
  }

  if (!g.actionClassName.str) {
    astParseError("you must specify a context class; for example:\n"
                  "  context_class Context : public UserActions {};\n");
  }

  if (env.errors) {
    astParseError("halting due to previously reported errors");
  }
}


// validate 'name'
Terminal *astParseToken(Environment &env, LocString const &name)
{
  Terminal *t = env.g.findTerminal(name);
  if (!t) {
    astParseError(name, "undeclared token");
  }
  return t;
}


// needed to ensure the GrowArray below has its values initialized
// to false when the array expands
class InitFalseBool {
public:
  bool b;
public:
  InitFalseBool() : b(false) {}
};


void astParseTerminals(Environment &env, TF_terminals const &terms)
{
  // basic declarations
  {
    int maxCode = 0;
    GrowArray<InitFalseBool> codeHasTerm(200);
    FOREACH_ASTLIST(TermDecl, terms.decls, iter) {
      TermDecl const &term = *(iter.data());

      // process the terminal declaration
      int code = term.code;
      StringRef name = term.name;
      trace("grampar") << "token: code=" << code
                       << ", name=" << name << std::endl;

      if (!env.g.declareToken(term.name, code, term.alias)) {
        astParseError(term.name, "token already declared");
      }

      // track what terminals have codes
      maxCode = max(code, maxCode);
      codeHasTerm.ensureIndexDoubler(code);
      codeHasTerm[code].b = true;
    }

    // fill in any gaps in the code space; this is required because
    // later analyses assume the terminal code space is dense
    SourceLoc dummyLoc(HERE_SOURCELOC);
    for (int i=0; i<maxCode; i++) {
      if (!codeHasTerm[i].b) {
        LocString dummy(dummyLoc, grammarStringTable.add(
          sm_stringc << "__dummy_filler_token" << i));
        env.g.declareToken(dummy, i, dummy);
      }
    }
  }

  // type annotations
  {
    FOREACH_ASTLIST(TermType, terms.types, iter) {
      TermType const &type = *(iter.data());
      trace("grampar") << "token type: name=" << type.name
                       << ", type=" << type.type << std::endl;

      // look up the name
      Terminal *t = astParseToken(env, type.name);
      if (t->type) {
        astParseError(type.name, "this token already has a type");
      }

      // annotate with declared type
      t->type = type.type;

      // parse the dup/del/merge spec
      astParseDDM(env, t, type.funcs);
    }
  }

  // precedence specifications
  {
    FOREACH_ASTLIST(PrecSpec, terms.prec, iter) {
      PrecSpec const &spec = *(iter.data());

      FOREACH_ASTLIST(LocString, spec.tokens, tokIter) {
        LocString const &tokName = *(tokIter.data());
        trace("grampar") << "prec: " << toString(spec.kind)
                         << " " << spec.prec << " " << tokName;

        // look up the token
        Terminal *t = astParseToken(env, tokName);
        if (t->precedence) {
          astParseError(tokName,
            sm_stringc << tokName << " already has a specified precedence");
        }

        if (spec.prec == 0) {
          // 0 means precedence isn't specified
          astParseError(tokName,
            "you can't use 0 as a precedence level, because that value "
            "is used internally to mean something else");
        }

        // apply spec
        t->precedence = spec.prec;
        t->associativity = spec.kind;
      }
    }
  }
}


void astParseDDM(Environment &env, Symbol *sym,
                 ASTList<SpecFunc> const &funcs)
{
  Terminal *term = sym->ifTerminal();
  Nonterminal *nonterm = sym->ifNonterminal();

  FOREACH_ASTLIST(SpecFunc, funcs, iter) {
    SpecFunc const &func = *(iter.data());
    int numFormals = func.formals.count();

    // decide what to do based on the name

    if (func.name.equals("dup")) {
      if (numFormals != 1) {
        astParseError(func.name, "'dup' function must have one formal parameter");
      }
      sym->dupParam = func.nthFormal(0);
      sym->dupCode = func.code;
    }

    else if (func.name.equals("del")) {
      if (numFormals == 0) {
        // not specified is ok, since it means the 'del' function
        // doesn't use its parameter
        sym->delParam = NULL;
      }
      else if (numFormals == 1) {
        sym->delParam = func.nthFormal(0);
      }
      else {
        astParseError(func.name, "'del' function must have either zero or one formal parameters");
      }
      sym->delCode = func.code;
    }

    else if (func.name.equals("merge")) {
      if (nonterm) {
        if (numFormals != 2) {
          astParseError(func.name, "'merge' function must have two formal parameters");
        }
        nonterm->mergeParam1 = func.nthFormal(0);
        nonterm->mergeParam2 = func.nthFormal(1);
        nonterm->mergeCode = func.code;
      }
      else {
        astParseError(func.name, "'merge' can only be applied to nonterminals");
      }
    }

    else if (func.name.equals("keep")) {
      if (nonterm) {
        if (numFormals != 1) {
          astParseError(func.name, "'keep' function must have one formal parameter");
        }
        nonterm->keepParam = func.nthFormal(0);
        nonterm->keepCode = func.code;
      }
      else {
        astParseError(func.name, "'keep' can only be applied to nonterminals");
      }
    }

    else if (func.name.equals("classify")) {
      if (term) {
        if (numFormals != 1) {
          astParseError(func.name, "'classify' function must have one formal parameter");
        }
        term->classifyParam = func.nthFormal(0);
        term->classifyCode = func.code;
      }
      else {
        astParseError(func.name, "'classify' can only be applied to terminals");
      }
    }

    else if (func.name.equals("maximal")) {
      if (nonterm) {
        nonterm->maximal = true;     // function body has no meaning
      }
      else {
        astParseError(func.name, "'maximal' can only be applied to nonterminals");
      }
    }

    else {
      astParseError(func.name,
        sm_stringc << "unrecognized spec function \"" << func.name << "\"");
    }
  }
}


void addDefaultTypesActions(Grammar &g, GrammarAST *ast)
{
  // language defaults
  StringRef defaultType, defaultAction;
  if (g.targetLang.equals("OCaml")) {
    defaultType = grammarStringTable.add("unit");
    defaultAction = grammarStringTable.add("()");
  }
  else /*C*/ {
    defaultType = grammarStringTable.add("void");
    defaultAction = grammarStringTable.add("return;");
  }

  // hook to allow me to force defaults everywhere (this is useful
  // when I want to try a grammar written for one language using
  // another language's core)
  bool forceDefaults = tracingSys("forceDefaultActions");

  // iterate over nonterminals
  FOREACH_ASTLIST_NC(TopForm, ast->forms, iter) {
    if (!iter.data()->isTF_nonterm()) { continue; }
    TF_nonterm *nt = iter.data()->asTF_nonterm();

    // default type
    if (forceDefaults || nt->type.isNull()) {
      nt->type.str = defaultType;
    }

    // iterate over productions
    FOREACH_ASTLIST_NC(ProdDecl, nt->productions, iter2) {
      ProdDecl *pd = iter2.data();

      // default action
      if (forceDefaults || pd->actionCode.isNull()) {
        pd->actionCode.str = defaultAction;
      }

      if (forceDefaults) {
        // clear RHSElt tags, since otherwise the lack of types
        // will provoke errors; and default actions don't refer to
        // the RHSElts anyway
        StringRef empty = grammarStringTable.add("");
        FOREACH_ASTLIST_NC(RHSElt, pd->rhs, iter3) {
          ASTSWITCH(RHSElt, iter3.data()) {
            ASTCASE(RH_name, n)
              n->tag.str = empty;

            ASTNEXT(RH_sm_string, s)
              s->tag.str = empty;

            ASTENDCASED
          }
        }
      }
    }
  }
}


void synthesizeStartRule(Grammar &g, GrammarAST *ast)
{
  // get the first nonterminal; this is the user's start symbol
  TF_nonterm *firstNT = ast->firstNT;

  // find the name of the user's EOF token
  TermDecl const *eof = NULL;
  FOREACH_ASTLIST(TermDecl, ast->terms->decls, iter) {
    if (iter.data()->code == 0) {
      eof = iter.data();
      break;
    }
  }
  if (!eof) {
    astParseError("you have to have an EOF token, with code 0");
  }

  // build a start production
  RHSElt *rhs1 = new RH_name(LIT_STR("top").clone(), firstNT->name.clone());
  RHSElt *rhs2 = new RH_name(LIT_STR("").clone(), eof->name.clone());
  ASTList<RHSElt> *rhs = new ASTList<RHSElt>();
  rhs->append(rhs1);
  rhs->append(rhs2);
  char const *action = g.targetLang.equals("OCaml")? " top " :
                       firstNT->type.equals("void")? " return; " :
                                                     " return top; ";
  ProdDecl *startProd = new ProdDecl(rhs, LIT_STR(action).clone());

  // build an even earlier start symbol
  TF_nonterm *earlyStartNT
    = new TF_nonterm(
        LIT_STR("__EarlyStartSymbol").clone(),   // name
        firstNT->type.clone(),                   // type
        NULL,                                    // empty list of functions
        new ASTList<ProdDecl>(startProd),        // productions
        NULL                                     // subsets
      );

  // put it into the AST
  ast->forms.prepend(earlyStartNT);
}


void astParseNonterm(Environment &env, TF_nonterm const *nt)
{
  LocString const &name = nt->name;

  // get the Grammar object that represents the nonterminal
  Nonterminal *nonterm = env.g.findNonterminal(name);
  xassert(nonterm);

  nonterm->type = nt->type;

  // iterate over the productions
  FOREACH_ASTLIST(ProdDecl, nt->productions, iter) {
    astParseProduction(env, nonterm, iter.data());
  }

  // parse dup/del/merge
  astParseDDM(env, nonterm, nt->funcs);

  // record subsets
  {
    FOREACH_ASTLIST(LocString, nt->subsets, iter) {
      LocString const *ls = iter.data();
      Nonterminal *sub = env.g.findNonterminal(*ls);
      if (!sub) {
        astParseError(*ls, "nonexistent nonterminal");
      }

      // note that, since context-free language inclusion is
      // undecidable (Hopcroft/Ullman), we can't actually check that
      // the given nonterminals really are in the subset relation
      nonterm->subsets.prepend(sub);
    }
  }
}


void astParseProduction(Environment &env, Nonterminal *nonterm,
                        ProdDecl const *prodDecl)
{
  // is this the special start symbol I inserted?
  bool synthesizedStart = nonterm->name.equals("__EarlyStartSymbol");

  // build a production; use 'this' as the tag for LHS elements
  Production *prod = new Production(nonterm, "this");

  // put the code into it
  prod->action = prodDecl->actionCode;

  // deal with RHS elements
  FOREACH_ASTLIST(RHSElt, prodDecl->rhs, iter) {
    RHSElt const *n = iter.data();
    LocString symName;
    LocString symTag;
    bool isString = false;
    bool isPrec = false;

    // pull various info out of the AST node
    ASTSWITCHC(RHSElt, n) {
      ASTCASEC(RH_name, tname) {
        symName = tname->name;
        symTag = tname->tag;
      }

      ASTNEXTC(RH_sm_string, ts) {
        symName = ts->str;
        symTag = ts->tag;
        isString = true;
      }

      ASTNEXTC(RH_prec, p) {
        // apply the specified precedence
        prod->precedence = astParseToken(env, p->tokName)->precedence;

        // and require that this is the last RHS element
        iter.adv();
        if (!iter.isDone()) {
          astParseError(p->tokName,
            "precedence spec must be last thing in a production "
            "(before the action code)");
        }
        isPrec = true;
      }

      ASTENDCASECD
    }

    if (isPrec) {
      break;     // last element anyway
    }

    // see which (if either) thing this name already is
    Terminal *term = env.g.findTerminal(symName);
    Nonterminal *nonterm = env.g.findNonterminal(symName);
    xassert(!( term && nonterm ));     // better not be both!

    // syntax rules
    if (isString  &&  !term) {
      astParseError(symName, "terminals must be declared");
    }

    if (!term && !nonterm) {
      astParseErrorCont(env, symName, "undeclared symbol");

      // synthesize one anyway so we can find more errors
      nonterm = env.g.getOrMakeNonterminal(symName);
    }

    if (term && term->termIndex==0 && !synthesizedStart) {
      astParseError(symName, "you cannot use the EOF token in your rules");
    }

    if (symTag.equals("loc")) {
      // bad because loc is the name of the automatically-propagated
      // source location information
      astParseErrorCont(env, symTag, "cannot use \"loc\" as a tag");
    }

    // whenever we see a terminal, copy its precedence spec to
    // the production; thus, the last symbol appearing in the
    // production will be the one that gives the precedence
    if (term) {
      prod->precedence = term->precedence;
    }

    // decide which symbol to put in the production
    Symbol *s;
    if (nonterm) {
      s = nonterm;            // could do these two with a bitwise OR
    }                         // if I were feeling extra clever today
    else {
      s = term;
    }

    if (s->isEmptyString) {
      // "empty" is a syntactic convenience; it doesn't get
      // added to the production
    }
    else {
      // add it to the production
      prod->append(s, symTag);
    }
  }

  // after constructing the production we need to do this
  // update: no we don't -- GrammarAnalysis takes care of it (and
  // complains if we do)
  //prod->finished();

  // add production to grammar
  env.g.addProduction(prod);
}


// ----------------------- parser support ---------------------
// Bison parser calls this to get a token
int grampar_yylex(YYSTYPE *lvalp, void *parseParam)
{
  ParseParams *par = (ParseParams*)parseParam;
  GrammarLexer &lexer = par->lexer;

  int code = lexer.yylexInc();

  try {
    // yield semantic values for some things
    // note that the yielded semantic value must be consistent with
    // what is declared for these token types in grampar.y
    switch (code) {
      case TOK_INTEGER:
        lvalp->num = lexer.integerLiteral;
        break;

      case TOK_STRING:
        lvalp->str = new LocString(lexer.curLoc(), lexer.sm_stringLiteral);
        break;

      case TOK_NAME:
        lvalp->str = new LocString(lexer.curLoc(), lexer.curToken());
        break;

      case TOK_LIT_CODE:
        lvalp->str = new LocString(lexer.curLoc(), lexer.curFuncBody());
        break;

      default:
        lvalp->str = NULL;        // any attempt to use will segfault
    }
  }
  catch (xBase &x) {
    // e.g. malformed fundecl
    std::cout << lexer.curLocStr() << ": " << x << std::endl;

    // optimistically try just skipping the bad token
    return grampar_yylex(lvalp, parseParam);
  }

  return code;
}


void grampar_yyerror(char const *message, void *parseParam)
{
  ParseParams *par = (ParseParams*)parseParam;
  std::cout << par->lexer.curLocStr() << ": " << message << std::endl;
}


// ---------------------- merging -----------------------
void mergeContext(GrammarAST *base, TF_context * /*owner*/ ext)
{
  // do simple append, since the grammar parser above knows how
  // to handle multiple context classes
  base->forms.append(ext);

  #if 0
  // find 'base' context
  TF_context *baseContext = NULL;
  FOREACH_ASTLIST_NC(TopForm, base->forms, iter) {
    if (iter.data()->isTF_context()) {
      baseContext = iter.data()->asTF_context();
      break;
    }
  }

  if (!baseContext) {
    // base does not have a context class, so 'ext' becomes it
    base->forms.append(ext);
  }

  else if (baseContext->name.str == ext->name.str) {
    // same name; I'd like to append the code to what's already
    // there, but that's tricky because the location won't
    // be right..
    astParseError(ext->name, "context append not implemented");
  }

  else {
    // different name, replace the old
    base->forms.removeItem(baseContext);
    delete baseContext;
    base->forms.append(ext);
  }
  #endif // 0
}


void mergeOption(GrammarAST *base, TF_option * /*owner*/ ext)
{
  // find option with the same name
  FOREACH_ASTLIST_NC(TopForm, base->forms, iter) {
    if (!iter.data()->isTF_option()) continue;
    TF_option *op = iter.data()->asTF_option();

    if (op->name.str == ext->name.str) {
      // replace the old value
      op->value = ext->value;
      delete ext;
      return;
    }
  }

  // otherwise, just add the new option
  base->forms.append(ext);
}


void mergeTerminals(GrammarAST *base, TF_terminals * /*owner*/ ext)
{
  FOREACH_ASTLIST_NC(TopForm, base->forms, iter) {
    if (iter.data()->isTF_terminals()) {
      TF_terminals *t = iter.data()->asTF_terminals();

      // there's no point to changing codes, so all the
      // TermDecls just get added (collisions are detected
      // later, during AST parsing)
      t->decls.concat(ext->decls);

      // in fact, I'll do the same for the others, even though
      // it might make sense to do some replacement; my immediate
      // needs don't include replacement at this level
      t->types.concat(ext->types);
      t->prec.concat(ext->prec);

      delete ext;
      return;
    }
  }

  // no TF_terminals in 'base'.. unusual, but easy to handle
  base->forms.append(ext);
}


void mergeSpecFunc(TF_nonterm *base, SpecFunc * /*owner*/ ext)
{
  // find an existing spec func with the same name
  FOREACH_ASTLIST_NC(SpecFunc, base->funcs, iter) {
    SpecFunc *f = iter.data();
    if (f->name.str == ext->name) {
      // replace the old code with the extension code
      base->funcs.removeItem(f);
      delete f;
      break;
    }
  }

  // just add it
  base->funcs.append(ext);
}


bool equalRHSElt(RHSElt const *elt1, RHSElt const *elt2)
{
  if (elt1->kind() != elt2->kind()) {
    return false;
  }

  // if the RHS names a terminal, this isn't perfect because one might
  // use an alias.. but I don't have the necessary information to detect
  // that, since I haven't yet computed the associated Symbols
  if (elt1->isRH_name()) {
    return elt1->asRH_nameC()->name.str == elt2->asRH_nameC()->name.str;
  }
  if (elt1->isRH_sm_string()) {
    return elt1->asRH_sm_stringC()->str.str == elt2->asRH_sm_stringC()->str.str;
  }
  if (elt1->isRH_prec()) {
    // this means you can't change the precedence..
    return elt1->asRH_precC()->tokName.str == elt2->asRH_precC()->tokName.str;
  }

  xfailure("unknown RHSElt kind");
  return false;     // silence warning
}


bool equalRHS(ProdDecl const *prod1, ProdDecl const *prod2)
{
  if (prod1->rhs.count() != prod2->rhs.count()) {
    return false;
  }

  for (ASTListIter<RHSElt> iter1(prod1->rhs), iter2(prod2->rhs);
       !iter1.isDone(); iter1.adv(), iter2.adv()) {
    if (!equalRHSElt(iter1.data(), iter2.data())) {
      return false;
    }
  }
  return true;
}


void mergeProduction(TF_nonterm *base, ProdDecl *ext)
{
  // look for a production with an identical RHS
  FOREACH_ASTLIST_NC(ProdDecl, base->productions, iter) {
    ProdDecl *prod = iter.data();

    // check RHSs for equality
    if (equalRHS(prod, ext)) {
      // replace old with new
      base->productions.removeItem(prod);
      delete prod;
      break;
    }
  }

  // add the production
  base->productions.append(ext);
}


void mergeNonterminal(GrammarAST *base, TF_nonterm * /*owner*/ ext)
{
  // find an existing nonterminal with the same name
  TF_nonterm *exist = NULL;
  FOREACH_ASTLIST_NC(TopForm, base->forms, iter) {
    if (iter.data()->isTF_nonterm() &&
        iter.data()->asTF_nonterm()->name.str == ext->name) {
      exist = iter.data()->asTF_nonterm();
    }
  }

  if (!exist) {
    // no pre-existing, just append it
    base->forms.append(ext);
    return;
  }

  // make sure the types agree
  if (exist->type.str != ext->type) {
    astParseError(ext->type, "cannot redefine the type of a nonterminal");
  }

  // merge the spec funcs
  while (ext->funcs.isNotEmpty()) {
    mergeSpecFunc(exist, ext->funcs.removeFirst());
  }

  // merge the productions
  while (ext->productions.isNotEmpty()) {
    mergeProduction(exist, ext->productions.removeFirst());
  }

  delete ext;
}


void mergeGrammar(GrammarAST *base, GrammarAST *ext)
{
  // work through all the forms in 'ext', removing each
  // one; it will then either be added to 'base', or
  // discarded entirely
  while (ext->forms.isNotEmpty()) {
    TopForm *form = ext->forms.removeFirst();

    ASTSWITCH(TopForm, form) {
      ASTCASE(TF_context, c) {
        mergeContext(base, c);
      }

      ASTNEXT(TF_verbatim, v) {
        // verbatims simply accumulate
        base->forms.append(v);
      }

      ASTNEXT(TF_option, op) {
        mergeOption(base, op);
      }

      ASTNEXT(TF_terminals, t) {
        mergeTerminals(base, t);
      }

      ASTNEXT(TF_nonterm, n) {
        mergeNonterminal(base, n);
      }

      ASTDEFAULT {
        xfailure("doh");
      }

      ASTENDCASE
    }
  }
}


// ---------------- external interface -------------------
bool isGramlexEmbed(int code);     // defined in gramlex.lex

GrammarAST *parseGrammarFile(char const *fname, bool useML)
{
  #ifndef NDEBUG
  if (tracingSys("yydebug")) {
    yydebug = true;    // this flag goes away when NDEBUG is specified..
  }
  #endif // NDEBUG

  // open input file
  Owner<std::ifstream> in;
  if (fname == NULL) {
    fname = "<stdin>";
  }
  else {
    in = new std::ifstream(fname);
    if (!*in) {
      xsyserror("open", sm_stringc << "error opening input file " << fname);
    }
  }

  // choose embedded language
  EmbeddedLang *embed = NULL;
  if (useML) {
    embed = new MLSubstrate;
  }

  // build lexer
  GrammarLexer lexer(isGramlexEmbed,
                     grammarStringTable,
                     fname,
                     in.xfr(),
                     embed);
  if (embed) {
    // install the refined error reporter
    embed->err = &lexer.altReporter;
  }

  ParseParams params(lexer);

  traceProgress() << "parsing grammar source: " << fname << std::endl;
  int retval = grampar_yyparse(&params);
  if (retval==0 && lexer.errors==0) {
    GrammarAST *ret = params.treeTop;

    if (tracingSys("printGrammarAST")) {
      // print AST
      std::cout << "AST:\n";
      ret->debugPrint(std::cout, 2);
    }

    return ret;
  }
  else {
    xbase("parsing finished with an error");
    return NULL;     // silence warning
  }
}


void parseGrammarAST(Grammar &g, GrammarAST *treeTop)
{
  setAnnotations(treeTop);

  // look at TF_options before synthesizing start rule,
  // so we can know what language is the target
  astParseOptions(g, treeTop);

  // fill in default types and actions
  addDefaultTypesActions(g, treeTop);

  // synthesize a rule "TrueStart -> Start EOF"
  synthesizeStartRule(g, treeTop);

  // parse the AST into a Grammar
  traceProgress() << "parsing grammar AST..\n";
  astParseGrammar(g, treeTop);

  // then check grammar properties; throws exception
  // on failure
  traceProgress() << "beginning grammar analysis..\n";
  g.checkWellFormed();
}


void readGrammarFile(Grammar &g, char const *fname)
{
  // make sure the tree gets deleted
  Owner<GrammarAST> treeTop(parseGrammarFile(fname, false /*useML*/));

  parseGrammarAST(g, treeTop);

  treeTop.del();

  // hmm.. I'd like to restore this functionality...
  //if (ASTNode::nodeCount > 0) {
  //  std::cout << "leaked " << ASTNode::nodeCount << " AST nodes\n";
  //}
}


// ----------------------- test code -----------------------
#ifdef TEST_GRAMPAR

#include "sm_bflatten.h"
#include <stdlib.h>       // system

int main(int argc, char **argv)
{
  if (argc < 2) {
    std::cout << "usage: " << argv[0] << " [-tr flags] filename.gr\n";
    std::cout << "  interesting trace flags:\n";
    std::cout << "    keep-tmp      do not delete the temporary files\n";
    //std::cout << "    cat-grammar   print the ascii rep to the screen\n";
    return 0;
  }

  traceAddSys("progress");
  TRACE_ARGS();

  bool printCode = true;

  // read the file
  Grammar g1;
  readGrammarFile(g1, argv[1]);

  // and print the grammar
  char const g1Fname[] = "grammar.g1.tmp";
  traceProgress() << "printing initial grammar to " << g1Fname << "\n";
  {
    std::ofstream out(g1Fname);
    g1.printSymbolTypes(out);
    g1.printProductions(out, printCode);
  }

  //if (tracingSys("cat-grammar")) {
    system("cat grammar.g1.tmp");
  //}

  // before using 'xfer' we have to tell it about the sm_string table
  flattenStrTable = &grammarStringTable;

  // write it to a binary file
  char const binFname[] = "grammar.bin.tmp";
  traceProgress() << "writing initial grammar to " << binFname << "\n";
  {
    BFlatten flat(binFname, false /*reading*/);
    g1.xfer(flat);
  }

  // read it back
  traceProgress() << "reading grammar from " << binFname << "\n";
  Grammar g2;
  {
    BFlatten flat(binFname, true /*reading*/);
    g2.xfer(flat);
  }

  // print that too
  char const g2Fname[] = "grammar.g2.tmp";
  traceProgress() << "printing just-read grammar to " << g2Fname << "\n";
  {
    std::ofstream out(g2Fname);
    g2.printSymbolTypes(out);
    g2.printProductions(out, printCode);
  }

  // compare the two written files
  int result = system(sm_stringc << "diff " << g1Fname << " " << g2Fname);
  if (result != 0) {
    std::cout << "the two ascii representations differ!!\n";
    return 4;
  }

  // remove the temp files
  if (!tracingSys("keep-tmp")) {
    remove(g1Fname);
    remove(g2Fname);
    remove(binFname);
  }

  std::cout << "successfully parsed, printed, wrote, and read a grammar!\n";
  return 0;
}

#endif // TEST_GRAMPAR
