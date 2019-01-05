Package: src/packages/ncurses.fdoc


===============
ncurses binding
===============

================ ================================
key              file                             
================ ================================
ncurses.flx      share/lib/std/io/ncurses.flx     
ncurses_01.flx   $PWD/src/examples/ncurses_01.flx 
ncurses_02.flx   $PWD/src/examples/ncurses_02.flx 
ncurses_03.flx   $PWD/src/examples/ncurses_03.flx 
unix_ncurses.fpc $PWD/src/config/unix/ncurses.fpc 
================ ================================

Expected to run on Unix platforms only.


.. index:: Ncurses(class)
.. index:: WINDOW(type)
.. index:: SCREEN(type)
.. index:: NCURSES_SCREEN_CB(type)
.. index:: NCURSES_WINDOW_CB(type)
.. index:: NCURSES_ATTR_T(type)
.. index:: def(type)
.. index:: def(type)
.. index:: chtype(ctor)
.. index:: chtype(ctor)
.. index:: char(ctor)
.. index:: int(ctor)
.. index:: stdscr(const)
.. index:: addch(gen)
.. index:: addchnstr(gen)
.. index:: addchstr(gen)
.. index:: addnstr(gen)
.. index:: addstr(gen)
.. index:: attroff(gen)
.. index:: attron(gen)
.. index:: attrset(gen)
.. index:: attr_get(gen)
.. index:: attr_off(gen)
.. index:: attr_on(gen)
.. index:: attr_set(gen)
.. index:: baudrate(gen)
.. index:: beep(gen)
.. index:: bkgd(gen)
.. index:: bkgdset(gen)
.. index:: border(gen)
.. index:: box(gen)
.. index:: can_change_color(gen)
.. index:: cbreak(gen)
.. index:: chgat(gen)
.. index:: clear(gen)
.. index:: clearok(gen)
.. index:: clrtobot(gen)
.. index:: clrtoeol(gen)
.. index:: color_content(gen)
.. index:: color_set(gen)
.. index:: COLOR_PAIR(gen)
.. index:: copywin(gen)
.. index:: curs_set(gen)
.. index:: def_prog_mode(gen)
.. index:: def_shell_mode(gen)
.. index:: delay_output(gen)
.. index:: delch(gen)
.. index:: delscreen(proc)
.. index:: delwin(gen)
.. index:: deleteln(gen)
.. index:: derwin(gen)
.. index:: doupdate(gen)
.. index:: dupwin(gen)
.. index:: echo(gen)
.. index:: echochar(gen)
.. index:: erase(gen)
.. index:: endwin(gen)
.. index:: erasechar(gen)
.. index:: filter(gen)
.. index:: flash(gen)
.. index:: flushinp(gen)
.. index:: getbkgd(gen)
.. index:: getch(gen)
.. index:: getnstr(gen)
.. index:: getstr(gen)
.. index:: halfdelay(gen)
.. index:: has_colors(gen)
.. index:: has_ic(gen)
.. index:: has_il(gen)
.. index:: hline(gen)
.. index:: idcok(gen)
.. index:: idlok(gen)
.. index:: immedok(gen)
.. index:: inch(gen)
.. index:: inchnstr(gen)
.. index:: inchstr(gen)
.. index:: initscr(gen)
.. index:: init_color(gen)
.. index:: init_pair(gen)
.. index:: innstr(gen)
.. index:: insch(gen)
.. index:: insdelln(gen)
.. index:: insertln(gen)
.. index:: insnstr(gen)
.. index:: insstr(gen)
.. index:: instr(gen)
.. index:: intrflush(gen)
.. index:: isendwin(gen)
.. index:: is_linetouched(gen)
.. index:: is_wintouched(gen)
.. index:: keyname(gen)
.. index:: keypad(gen)
.. index:: killchar(gen)
.. index:: leaveok(gen)
.. index:: longname(gen)
.. index:: meta(gen)
.. index:: move(gen)
.. index:: mvaddch(gen)
.. index:: mvaddchnstr(gen)
.. index:: mvaddchstr(gen)
.. index:: mvaddnstr(gen)
.. index:: mvaddstr(gen)
.. index:: mvchgat(gen)
.. index:: mvcur(gen)
.. index:: mvdelch(gen)
.. index:: mvderwin(gen)
.. index:: mvgetch(gen)
.. index:: mvgetnstr(gen)
.. index:: mvgetstr(gen)
.. index:: mvhline(gen)
.. index:: mvinch(gen)
.. index:: mvinchnstr(gen)
.. index:: mvinchstr(gen)
.. index:: mvinnstr(gen)
.. index:: mvinsch(gen)
.. index:: mvinsnstr(gen)
.. index:: mvinsstr(gen)
.. index:: mvinstr(gen)
.. index:: mvprintw(proc)
.. index:: mvvline(gen)
.. index:: mvwaddch(gen)
.. index:: mvwaddchnstr(gen)
.. index:: mvwaddchstr(gen)
.. index:: mvwaddnstr(gen)
.. index:: mvwaddstr(gen)
.. index:: mvwchgat(gen)
.. index:: mvwdelch(gen)
.. index:: mvwgetch(gen)
.. index:: mvwgetnstr(gen)
.. index:: mvwgetstr(gen)
.. index:: mvwhline(gen)
.. index:: mvwin(gen)
.. index:: mvwinch(gen)
.. index:: mvwinchnstr(gen)
.. index:: mvwinchstr(gen)
.. index:: mvwinnstr(gen)
.. index:: mvwinsch(gen)
.. index:: mvwinsnstr(gen)
.. index:: mvwinsstr(gen)
.. index:: mvwinstr(gen)
.. index:: mvwprintw(proc)
.. index:: mvwvline(gen)
.. index:: napms(gen)
.. index:: newpad(gen)
.. index:: newterm(gen)
.. index:: newwin(gen)
.. index:: nl(gen)
.. index:: nocbreak(gen)
.. index:: nodelay(gen)
.. index:: noecho(gen)
.. index:: nonl(gen)
.. index:: noqiflush(gen)
.. index:: noraw(gen)
.. index:: notimeout(gen)
.. index:: overlay(gen)
.. index:: overwrite(gen)
.. index:: pair_content(gen)
.. index:: PAIR_NUMBER(gen)
.. index:: pechochar(gen)
.. index:: pnoutrefresh(gen)
.. index:: prefresh(gen)
.. index:: printw(proc)
.. index:: putwin(gen)
.. index:: qiflush(gen)
.. index:: raw(gen)
.. index:: redrawwin(gen)
.. index:: refresh(gen)
.. index:: resetty(gen)
.. index:: reset_prog_mode(gen)
.. index:: reset_shell_mode(gen)
.. index:: savetty(gen)
.. index:: scr_dump(gen)
.. index:: scr_init(gen)
.. index:: scrl(gen)
.. index:: scroll(gen)
.. index:: scrollok(gen)
.. index:: scr_restore(gen)
.. index:: scr_set(gen)
.. index:: setscrreg(gen)
.. index:: set_term(gen)
.. index:: slk_attroff(gen)
.. index:: slk_attr_off(gen)
.. index:: slk_attron(gen)
.. index:: slk_attr_on(gen)
.. index:: slk_attrset(gen)
.. index:: slk_attr(gen)
.. index:: slk_attr_set(gen)
.. index:: slk_clear(gen)
.. index:: slk_color(gen)
.. index:: slk_init(gen)
.. index:: slk_label(gen)
.. index:: slk_noutrefresh(gen)
.. index:: slk_refresh(gen)
.. index:: slk_restore(gen)
.. index:: slk_set(gen)
.. index:: slk_touch(gen)
.. index:: standout(gen)
.. index:: standend(gen)
.. index:: start_color(gen)
.. index:: subpad(gen)
.. index:: subwin(gen)
.. index:: syncok(gen)
.. index:: termattrs(gen)
.. index:: termname(gen)
.. index:: timeout(gen)
.. index:: touchline(gen)
.. index:: touchwin(gen)
.. index:: typeahead(gen)
.. index:: ungetch(gen)
.. index:: untouchwin(gen)
.. index:: use_env(gen)
.. index:: vidattr(gen)
.. index:: vline(gen)
.. index:: vwprintw(gen)
.. index:: vw_printw(gen)
.. index:: vwscanw(gen)
.. index:: vw_scanw(gen)
.. index:: waddch(gen)
.. index:: waddchnstr(gen)
.. index:: waddchstr(gen)
.. index:: waddnstr(gen)
.. index:: waddstr(gen)
.. index:: waddstr(proc)
.. index:: wattron(gen)
.. index:: wattroff(gen)
.. index:: wattrset(gen)
.. index:: wattr_get(gen)
.. index:: wattr_on(gen)
.. index:: wattr_off(gen)
.. index:: wattr_set(gen)
.. index:: wbkgd(gen)
.. index:: wbkgdset(gen)
.. index:: wborder(gen)
.. index:: wchgat(gen)
.. index:: wclear(gen)
.. index:: wclrtobot(gen)
.. index:: wclrtoeol(gen)
.. index:: wcolor_set(gen)
.. index:: wcursyncup(gen)
.. index:: wdelch(gen)
.. index:: wdeleteln(gen)
.. index:: wechochar(gen)
.. index:: werase(gen)
.. index:: wgetch(gen)
.. index:: wgetnstr(gen)
.. index:: wgetstr(gen)
.. index:: whline(gen)
.. index:: winch(gen)
.. index:: winchnstr(gen)
.. index:: winchstr(gen)
.. index:: winnstr(gen)
.. index:: winsch(gen)
.. index:: winsdelln(gen)
.. index:: winsertln(gen)
.. index:: winsnstr(gen)
.. index:: winsstr(gen)
.. index:: winstr(gen)
.. index:: wmove(gen)
.. index:: wnoutrefresh(gen)
.. index:: wprintw(proc)
.. index:: wredrawln(gen)
.. index:: wrefresh(gen)
.. index:: wscrl(gen)
.. index:: wsetscrreg(gen)
.. index:: wstandout(gen)
.. index:: wstandend(gen)
.. index:: wsyncdown(gen)
.. index:: wsyncup(gen)
.. index:: wtimeout(gen)
.. index:: wtouchln(gen)
.. index:: wvline(gen)
.. index:: tigetflag(gen)
.. index:: tigetnum(gen)
.. index:: tigetstr(gen)
.. index:: putp(gen)
.. index:: getattrs(gen)
.. index:: getcurx(gen)
.. index:: getcury(gen)
.. index:: getbegx(gen)
.. index:: getbegy(gen)
.. index:: getmaxx(gen)
.. index:: getmaxy(gen)
.. index:: getparx(gen)
.. index:: getpary(gen)
.. index:: is_term_resized(gen)
.. index:: keybound(gen)
.. index:: curses_version(gen)
.. index:: assume_default_colors(gen)
.. index:: define_key(gen)
.. index:: key_defined(gen)
.. index:: keyok(gen)
.. index:: resize_term(gen)
.. index:: resizeterm(gen)
.. index:: set_escdelay(gen)
.. index:: set_tabsize(gen)
.. index:: use_default_colors(gen)
.. index:: use_extended_names(gen)
.. index:: use_legacy_coding(gen)
.. index:: use_screen(gen)
.. index:: use_window(gen)
.. index:: wresize(gen)
.. index:: nofilter(proc)
.. index:: wgetparent(gen)
.. index:: is_cleared(gen)
.. index:: is_idcok(gen)
.. index:: is_idlok(gen)
.. index:: is_immedok(gen)
.. index:: is_keypad(gen)
.. index:: is_leaveok(gen)
.. index:: is_nodelay(gen)
.. index:: is_notimeout(gen)
.. index:: is_scrollok(gen)
.. index:: is_syncok(gen)
.. index:: wgetscrreg(gen)
.. index:: A_NORMAL(const)
.. index:: mmask_t(type)
.. index:: MEVENT(cstruct)
.. index:: BUTTON1_RELEASED(const)
.. index:: BUTTON1_PRESSED(const)
.. index:: BUTTON1_CLICKED(const)
.. index:: BUTTON1_DOUBLE_CLICKED(const)
.. index:: BUTTON1_TRIPLE_CLICKED(const)
.. index:: BUTTON2_RELEASED(const)
.. index:: BUTTON2_PRESSED(const)
.. index:: BUTTON2_CLICKED(const)
.. index:: BUTTON2_DOUBLE_CLICKED(const)
.. index:: BUTTON2_TRIPLE_CLICKED(const)
.. index:: BUTTON3_RELEASED(const)
.. index:: BUTTON3_PRESSED(const)
.. index:: BUTTON3_CLICKED(const)
.. index:: BUTTON3_DOUBLE_CLICKED(const)
.. index:: BUTTON3_TRIPLE_CLICKED(const)
.. index:: BUTTON4_RELEASED(const)
.. index:: BUTTON4_PRESSED(const)
.. index:: BUTTON4_CLICKED(const)
.. index:: BUTTON4_DOUBLE_CLICKED(const)
.. index:: BUTTON4_TRIPLE_CLICKED(const)
.. index:: BUTTON_CTRL(const)
.. index:: BUTTON_SHIFT(const)
.. index:: BUTTON_ALT(const)
.. index:: ALL_MOUSE_EVENTS(const)
.. index:: REPORT_MOUSE_POSITION(const)
.. index:: getmouse(gen)
.. index:: ungetmouse(gen)
.. index:: mousemask(gen)
.. index:: wenclose(gen)
.. index:: mouseinterval(gen)
.. index:: wmouse_trafo(gen)
.. index:: mouse_trafo(gen)
.. index:: ACS_ULCORNER(const)
.. index:: ACS_LLCORNER(const)
.. index:: ACS_URCORNER(const)
.. index:: ACS_LRCORNER(const)
.. index:: ACS_LTEE(const)
.. index:: ACS_RTEE(const)
.. index:: ACS_BTEE(const)
.. index:: ACS_TTEE(const)
.. index:: ACS_HLINE(const)
.. index:: ACS_VLINE(const)
.. index:: ACS_PLUS(const)
.. index:: ACS_S1(const)
.. index:: ACS_S9(const)
.. index:: ACS_DIAMOND(const)
.. index:: ACS_CKBOARD(const)
.. index:: ACS_DEGREE(const)
.. index:: ACS_PLMINUS(const)
.. index:: ACS_BULLET(const)
.. index:: ACS_LARROW(const)
.. index:: ACS_RARROW(const)
.. index:: ACS_DARROW(const)
.. index:: ACS_UARROW(const)
.. index:: ACS_BOARD(const)
.. index:: ACS_LANTERN(const)
.. index:: ACS_BLOCK(const)
.. index:: ACS_S3(const)
.. index:: ACS_S7(const)
.. index:: ACS_LEQUAL(const)
.. index:: ACS_GEQUAL(const)
.. index:: ACS_PI(const)
.. index:: ACS_NEQUAL(const)
.. index:: ACS_STERLING(const)
.. index:: ACS_BSSB(const)
.. index:: ACS_SSBB(const)
.. index:: ACS_BBSS(const)
.. index:: ACS_SBBS(const)
.. index:: ACS_SBSS(const)
.. index:: ACS_SSSB(const)
.. index:: ACS_SSBS(const)
.. index:: ACS_BSSS(const)
.. index:: ACS_BSBS(const)
.. index:: ACS_SBSB(const)
.. index:: ACS_SSSS(const)
.. index:: KEY_CODE_YES(const)
.. index:: KEY_MIN(const)
.. index:: KEY_BREAK(const)
.. index:: KEY_SRESET(const)
.. index:: KEY_RESET(const)
.. index:: KEY_DOWN(const)
.. index:: KEY_UP(const)
.. index:: KEY_LEFT(const)
.. index:: KEY_RIGHT(const)
.. index:: KEY_HOME(const)
.. index:: KEY_BACKSPACE(const)
.. index:: KEY_F0(const)
.. index:: KEY_F1(const)
.. index:: KEY_F2(const)
.. index:: KEY_F3(const)
.. index:: KEY_F4(const)
.. index:: KEY_F5(const)
.. index:: KEY_F6(const)
.. index:: KEY_F7(const)
.. index:: KEY_F8(const)
.. index:: KEY_F9(const)
.. index:: KEY_F10(const)
.. index:: KEY_F11(const)
.. index:: KEY_F12(const)
.. index:: KEY_DL(const)
.. index:: KEY_IL(const)
.. index:: KEY_DC(const)
.. index:: KEY_IC(const)
.. index:: KEY_EIC(const)
.. index:: KEY_CLEAR(const)
.. index:: KEY_EOS(const)
.. index:: KEY_EOL(const)
.. index:: KEY_SF(const)
.. index:: KEY_SR(const)
.. index:: KEY_NPAGE(const)
.. index:: KEY_PPAGE(const)
.. index:: KEY_STAB(const)
.. index:: KEY_CTAB(const)
.. index:: KEY_CATAB(const)
.. index:: KEY_ENTER(const)
.. index:: KEY_PRINT(const)
.. index:: KEY_LL(const)
.. index:: KEY_A1(const)
.. index:: KEY_A3(const)
.. index:: KEY_B2(const)
.. index:: KEY_C1(const)
.. index:: KEY_C3(const)
.. index:: KEY_BTAB(const)
.. index:: KEY_BEG(const)
.. index:: KEY_CANCEL(const)
.. index:: KEY_CLOSE(const)
.. index:: KEY_COMMAND(const)
.. index:: KEY_COPY(const)
.. index:: KEY_CREATE(const)
.. index:: KEY_END(const)
.. index:: KEY_EXIT(const)
.. index:: KEY_FIND(const)
.. index:: KEY_HELP(const)
.. index:: KEY_MARK(const)
.. index:: KEY_MESSAGE(const)
.. index:: KEY_MOVE(const)
.. index:: KEY_NEXT(const)
.. index:: KEY_OPEN(const)
.. index:: KEY_OPTIONS(const)
.. index:: KEY_PREVIOUS(const)
.. index:: KEY_REDO(const)
.. index:: KEY_REFERENCE(const)
.. index:: KEY_REFRESH(const)
.. index:: KEY_REPLACE(const)
.. index:: KEY_RESTART(const)
.. index:: KEY_RESUME(const)
.. index:: KEY_SAVE(const)
.. index:: KEY_SBEG(const)
.. index:: KEY_SCANCEL(const)
.. index:: KEY_SCOMMAND(const)
.. index:: KEY_SCOPY(const)
.. index:: KEY_SCREATE(const)
.. index:: KEY_SDC(const)
.. index:: KEY_SDL(const)
.. index:: KEY_SELECT(const)
.. index:: KEY_SEND(const)
.. index:: KEY_SEOL(const)
.. index:: KEY_SEXIT(const)
.. index:: KEY_SFIND(const)
.. index:: KEY_SHELP(const)
.. index:: KEY_SHOME(const)
.. index:: KEY_SIC(const)
.. index:: KEY_SLEFT(const)
.. index:: KEY_SMESSAGE(const)
.. index:: KEY_SMOVE(const)
.. index:: KEY_SNEXT(const)
.. index:: KEY_SOPTIONS(const)
.. index:: KEY_SPREVIOUS(const)
.. index:: KEY_SPRINT(const)
.. index:: KEY_SREDO(const)
.. index:: KEY_SREPLACE(const)
.. index:: KEY_SRIGHT(const)
.. index:: KEY_SRSUME(const)
.. index:: KEY_SSAVE(const)
.. index:: KEY_SSUSPEND(const)
.. index:: KEY_SUNDO(const)
.. index:: KEY_SUSPEND(const)
.. index:: KEY_UNDO(const)
.. index:: KEY_MOUSE(const)
.. index:: KEY_RESIZE(const)
.. index:: KEY_EVENT(const)
.. index:: KEY_MAX(const)
.. index:: LINES(fun)
.. index:: COLS(fun)
.. code-block:: felix

  //[ncurses.flx]
  // This library is licenced under FFAU
  class Ncurses 
  {
    requires package "ncurses";
    type WINDOW = "WINDOW*";
    type SCREEN = "SCREEN*";
    type NCURSES_SCREEN_CB = "NCURSE_SCREEN_CB";
    type NCURSES_WINDOW_CB = "NCURSE_WINDOW_CB";
    type NCURSES_ATTR_T = "NCURSES_ATTR_T";
  
    // hackery!
    typedef attr_t = uint;
    typedef chtype = uint;
    ctor chtype : int = "(unsigned int)$1"; 
    ctor chtype : char = "(unsigned int)$1"; 
    ctor char : chtype = "(char)$1";
    ctor int : chtype = "(int)$1";
    const stdscr : WINDOW = "stdscr";
  
    gen addch: chtype -> int;   // generated
    gen addchnstr: &chtype * int -> int;  // generated
    gen addchstr: &chtype -> int;   // generated
    gen addnstr: &char * int -> int;   // generated
    gen addstr: &char -> int;   // generated
    gen attroff: NCURSES_ATTR_T -> int;   // generated
    gen attron: NCURSES_ATTR_T -> int;   // generated
    gen attrset: NCURSES_ATTR_T -> int;   // generated
    gen attr_get: &attr_t * &short * address -> int; // generated
    gen attr_off: attr_t * address -> int;   // generated
    gen attr_on: attr_t * address -> int;   // generated
    gen attr_set: attr_t * short * address -> int;  // generated
    gen baudrate: unit -> int;    // implemented 
    gen beep : unit -> int;    // implemented 
    gen bkgd: chtype -> int;    // generated
    gen bkgdset: chtype -> void;    // generated
    gen border: chtype * chtype * chtype * chtype * chtype * chtype * chtype * chtype -> int; // generated
    gen box: WINDOW * chtype * chtype -> int;  // generated
    gen can_change_color: unit -> bool;   // implemented 
    gen cbreak: unit -> int;    // implemented 
    gen chgat: int * attr_t * short * address -> int; // generated
    gen clear: unit -> int;    // generated
    gen clearok: WINDOW * bool -> int;   // implemented 
    gen clrtobot: unit -> int;    // generated
    gen clrtoeol: unit -> int;    // generated
    gen color_content: short * &short * &short * &short -> int; // implemented 
    gen color_set: short * address -> int;   // generated
    gen COLOR_PAIR: int -> int;    // generated
    gen copywin: WINDOW * WINDOW * int * int * int * int * int * int * int -> int; // implemented 
    gen curs_set: int -> int;    // implemented 
    gen def_prog_mode: unit -> int;   // implemented 
    gen def_shell_mode: unit -> int;   // implemented 
    gen delay_output: int -> int;    // implemented 
    gen delch: unit -> int;    // generated
    proc delscreen: SCREEN ;   // implemented 
    gen delwin: WINDOW -> int;    // implemented 
    gen deleteln: unit -> int;    // generated
    gen derwin: WINDOW * int * int * int * int -> WINDOW; // implemented 
    gen doupdate: unit -> int;    // implemented 
    gen dupwin: WINDOW -> WINDOW;   // implemented 
    gen echo: unit -> int;     // implemented 
    gen echochar: chtype -> int;   // generated
    gen erase: unit -> int;    // generated
    gen endwin: unit -> int;    // implemented 
    gen erasechar: unit -> char;    // implemented 
    gen filter: unit -> void;    // implemented 
    gen flash: unit -> int;    // implemented 
    gen flushinp: unit -> int;    // implemented 
    gen getbkgd: WINDOW -> chtype;   // generated
    gen getch: unit -> int;    // generated
    gen getnstr: +char * int -> int;   // generated
    gen getstr: +char -> int;    // generated
  //  gen getwin: &FILE -> WINDOW;   // implemented 
    gen halfdelay: int -> int;    // implemented 
    gen has_colors: unit -> bool;    // implemented 
    gen has_ic: unit -> bool;    // implemented 
    gen has_il: unit -> bool;    // implemented 
    gen hline: chtype * int -> int;    // generated
    gen idcok: WINDOW * bool -> void;   // implemented 
    gen idlok: WINDOW * bool -> int;   // implemented 
    gen immedok: WINDOW * bool -> void;   // implemented 
    gen inch: unit -> chtype;    // generated
    gen inchnstr: &chtype * int -> int;   // generated
    gen inchstr: &chtype -> int;    // generated
    gen initscr: unit -> WINDOW;    // implemented 
    gen init_color: short * short * short * short -> int; // implemented 
    gen init_pair: short * short * short -> int;  // implemented 
    gen innstr: &char * int -> int;   // generated
    gen insch: chtype -> int;    // generated
    gen insdelln: int -> int;    // generated
    gen insertln: unit -> int;    // generated
    gen insnstr: &char * int -> int;   // generated
    gen insstr: &char -> int;   // generated
    gen instr: &char -> int;    // generated
    gen intrflush: WINDOW * bool -> int;   // implemented 
    gen isendwin: unit -> bool;    // implemented 
    gen is_linetouched: WINDOW * int -> bool;  // implemented 
    gen is_wintouched: WINDOW -> bool;   // implemented 
    gen keyname: int -> &char;  // implemented 
    gen keypad: WINDOW * bool -> int;   // implemented 
    gen killchar: unit -> char;    // implemented 
    gen leaveok: WINDOW * bool -> int;   // implemented 
    gen longname: unit -> &char;    // implemented 
    gen meta: WINDOW * bool -> int;   // implemented 
    gen move: int * int -> int;    // generated
    gen mvaddch: int * int * chtype -> int;  // generated
    gen mvaddchnstr: int * int * &chtype * int -> int; // generated
    gen mvaddchstr: int * int * &chtype -> int; // generated
    gen mvaddnstr: int * int * &char * int -> int; // generated
    gen mvaddstr: int * int * &char -> int;  // generated
    gen mvchgat: int * int * int * attr_t * short * address -> int; // generated
    gen mvcur: int * int * int * int -> int;   // implemented 
    gen mvdelch: int * int -> int;    // generated
    gen mvderwin: WINDOW * int * int -> int;  // implemented 
    gen mvgetch: int * int -> int;    // generated
    gen mvgetnstr: int * int * +char * int -> int;  // generated
    gen mvgetstr: int * int * +char -> int;   // generated
    gen mvhline: int * int * chtype * int -> int;  // generated
    gen mvinch: int * int -> chtype;   // generated
    gen mvinchnstr: int * int * &chtype * int -> int; // generated
    gen mvinchstr: int * int * &chtype -> int;  // generated
    gen mvinnstr: int * int * &char * int -> int;  // generated
    gen mvinsch: int * int * chtype -> int;   // generated
    gen mvinsnstr: int * int * &char * int -> int; // generated
    gen mvinsstr: int * int * &char -> int;  // generated
    gen mvinstr: int * int * &char -> int;   // generated
  //extern NCURSES_EXPORT(int) mvprintw (int * int * &char * ...)  // implemented 
  //  GCC_PRINTFLIKE(3 * 4);
  //extern NCURSES_EXPORT(int) mvscanw (int * int * &char * ...) // implemented 
  //  GCC_SCANFLIKE(3 * 4);
    proc mvprintw: int * int * string = '(void)mvprintw($1,$2,"%s",$1.c_str());';
  
    gen mvvline: int * int * chtype * int -> int;  // generated
    gen mvwaddch: WINDOW * int * int * chtype -> int; // generated
    gen mvwaddchnstr: WINDOW * int * int * &chtype * int -> int;// generated
    gen mvwaddchstr: WINDOW * int * int * &chtype -> int; // generated
    gen mvwaddnstr: WINDOW * int * int * &char * int -> int; // generated
    gen mvwaddstr: WINDOW * int * int * &char -> int; // generated
    gen mvwchgat: WINDOW * int * int * int * attr_t * short * address -> int;// generated
    gen mvwdelch: WINDOW * int * int -> int;  // generated
    gen mvwgetch: WINDOW * int * int -> int;  // generated
    gen mvwgetnstr: WINDOW * int * int * +char * int -> int; // generated
    gen mvwgetstr: WINDOW * int * int * +char -> int; // generated
    gen mvwhline: WINDOW * int * int * chtype * int -> int; // generated
    gen mvwin: WINDOW * int * int -> int;   // implemented 
    gen mvwinch: WINDOW * int * int -> chtype;   // generated
    gen mvwinchnstr: WINDOW * int * int * &chtype * int -> int; // generated
    gen mvwinchstr: WINDOW * int * int * &chtype -> int;  // generated
    gen mvwinnstr: WINDOW * int * int * &char * int -> int;  // generated
    gen mvwinsch: WINDOW * int * int * chtype -> int;  // generated
    gen mvwinsnstr: WINDOW * int * int * &char * int -> int; // generated
    gen mvwinsstr: WINDOW * int * int * &char -> int;  // generated
    gen mvwinstr: WINDOW * int * int * &char -> int;  // generated
  //extern NCURSES_EXPORT(int) mvwprintw (&WINDOW * int * int * &char * ...) // implemented 
  //  GCC_PRINTFLIKE(4 * 5);
  //extern NCURSES_EXPORT(int) mvwscanw (WINDOW * int * int * &char * ...) // implemented 
  //  GCC_SCANFLIKE(4 * 5);
    proc mvwprintw: WINDOW * int * int * string = '(void)mvwprintw($1,$2,$3,"%s",$4.c_str());';
  
    gen mvwvline: WINDOW * int * int * chtype * int -> int; // generated
    gen napms: int -> int;     // implemented 
    gen newpad: int * int -> WINDOW;    // implemented 
    gen newterm: string * ifile * ofile -> SCREEN = "newterm(strdup($1.c_str()),$2,$3)"; // implemented 
    //gen newterm: &char * &FILE * &FILE -> &SCREEN; // implemented 
    gen newwin: int * int * int * int -> WINDOW;   // implemented 
    gen nl: unit -> int;     // implemented 
    gen nocbreak: unit -> int;    // implemented 
    gen nodelay: WINDOW * bool -> int;   // implemented 
    gen noecho: unit -> int;    // implemented 
    gen nonl: unit -> int;     // implemented 
    gen noqiflush: unit -> void;    // implemented 
    gen noraw: unit -> int;    // implemented 
    gen notimeout: WINDOW * bool -> int;   // implemented 
    gen overlay: &WINDOW * WINDOW -> int;  // implemented 
    gen overwrite: &WINDOW * WINDOW -> int;  // implemented 
    gen pair_content: short * &short * &short -> int;  // implemented 
    gen PAIR_NUMBER: int -> int;    // generated
    gen pechochar: WINDOW * chtype -> int;  // implemented 
    gen pnoutrefresh: &WINDOW * int * int * int * int * int * int -> int;// implemented 
    gen prefresh: WINDOW * int * int * int * int * int * int -> int; // implemented 
  //extern NCURSES_EXPORT(int) printw (&char * ...)   // implemented 
  //  GCC_PRINTFLIKE(1 * 2);
  
    proc printw : string = '(void)printw("%s",$1.c_str());';
  
    gen putwin: WINDOW * &FILE -> int;   // implemented 
    gen qiflush: unit -> void;    // implemented 
    gen raw: unit -> int;     // implemented 
    gen redrawwin: WINDOW -> int;   // generated
    gen refresh: unit -> int;    // generated
    gen resetty: unit -> int;    // implemented 
    gen reset_prog_mode: unit -> int;   // implemented 
    gen reset_shell_mode: unit -> int;   // implemented 
  //   gen ripoffline (int * int: *)(WINDOW * int) -> int; // implemented 
    gen savetty: unit -> int;    // implemented 
  //extern NCURSES_EXPORT(int) scanw (&char * ...)  // implemented 
  //  GCC_SCANFLIKE(1 * 2);
    gen scr_dump: &char -> int;   // implemented 
    gen scr_init: &char -> int;   // implemented 
    gen scrl: int -> int;     // generated
    gen scroll: WINDOW -> int;    // generated
    gen scrollok: WINDOW * bool -> int;   // implemented 
    gen scr_restore: &char -> int;   // implemented 
    gen scr_set: &char -> int;   // implemented 
    gen setscrreg: int * int -> int;    // generated
    gen set_term: &SCREEN -> &SCREEN;   // implemented 
    gen slk_attroff: chtype -> int;   // implemented 
    gen slk_attr_off: attr_t * address -> int;  // generated:WIDEC
    gen slk_attron: chtype -> int;   // implemented 
    gen slk_attr_on: attr_t * address -> int;   // generated:WIDEC
    gen slk_attrset: chtype -> int;   // implemented 
    gen slk_attr: unit -> attr_t;    // implemented 
    gen slk_attr_set: attr_t * short * address -> int; // implemented 
    gen slk_clear: unit -> int;    // implemented 
    gen slk_color: short -> int;    // implemented 
    gen slk_init: int -> int;    // implemented 
    gen slk_label: int -> &char;    // implemented 
    gen slk_noutrefresh: unit -> int;   // implemented 
    gen slk_refresh: unit -> int;    // implemented 
    gen slk_restore: unit -> int;    // implemented 
    gen slk_set: int * &char * int -> int;  // implemented 
    gen slk_touch: unit -> int;    // implemented 
    gen standout: unit -> int;    // generated
    gen standend: unit -> int;    // generated
    gen start_color: unit -> int;    // implemented 
    gen subpad: WINDOW * int * int * int * int -> WINDOW; // implemented 
    gen subwin: WINDOW * int * int * int * int -> WINDOW; // implemented 
    gen syncok: WINDOW * bool -> int;   // implemented 
    gen termattrs: unit -> chtype;    // implemented 
    gen termname: unit -> &char;    // implemented 
    gen timeout: int -> void;    // generated
    gen touchline: WINDOW * int * int -> int;  // generated
    gen touchwin: WINDOW -> int;    // generated
    gen typeahead: int -> int;    // implemented 
    gen ungetch: int -> int;    // implemented 
    gen untouchwin: WINDOW -> int;   // generated
    gen use_env: bool -> void;    // implemented 
    gen vidattr: chtype -> int;    // implemented 
  //  gen vidputs (chtype * int: *)(int) -> int;  // implemented 
    gen vline: chtype * int -> int;    // generated
    gen vwprintw: WINDOW * &char * C_hack::va_list -> int; // implemented 
    gen vw_printw: WINDOW * &char * C_hack::va_list -> int; // generated
    gen vwscanw: WINDOW * &char * C_hack::va_list -> int; // implemented 
    gen vw_scanw: WINDOW * &char * C_hack::va_list -> int; // generated
    gen waddch: WINDOW * chtype -> int;  // implemented 
    gen waddchnstr: WINDOW * &chtype * int -> int; // implemented 
    gen waddchstr: WINDOW * &chtype -> int;  // generated
    gen waddnstr: WINDOW * &char * int -> int; // implemented 
    gen waddstr: WINDOW * &char -> int;  // generated
    proc waddstr: WINDOW * string = '(void)waddstr($1,$2.c_str());';
    gen wattron: WINDOW * int -> int;   // generated
    gen wattroff: WINDOW * int -> int;   // generated
    gen wattrset: WINDOW * int -> int;   // generated
    gen wattr_get: WINDOW * &attr_t * &short * address -> int; // generated
    gen wattr_on: WINDOW * attr_t * address -> int;  // implemented 
    gen wattr_off: WINDOW * attr_t * address -> int; // implemented 
    gen wattr_set: WINDOW * attr_t * short * address -> int; // generated
    gen wbkgd: WINDOW * chtype -> int;   // implemented 
    gen wbkgdset: WINDOW * chtype -> void;   // implemented 
    gen wborder: WINDOW * chtype * chtype * chtype * chtype * chtype * chtype * chtype * chtype -> int; // implemented 
    gen wchgat: WINDOW * int * attr_t * short * address -> int;// implemented 
    gen wclear: WINDOW -> int;    // implemented 
    gen wclrtobot: WINDOW -> int;   // implemented 
    gen wclrtoeol: WINDOW -> int;   // implemented 
    gen wcolor_set: &WINDOW * short * address -> int;  // implemented 
    gen wcursyncup: WINDOW -> void;   // implemented 
    gen wdelch: WINDOW -> int;    // implemented 
    gen wdeleteln: WINDOW -> int;   // generated
    gen wechochar: WINDOW * chtype -> int;  // implemented 
    gen werase: WINDOW -> int;    // implemented 
    gen wgetch: WINDOW -> int;    // implemented 
    gen wgetnstr: WINDOW * &char * int -> int;  // implemented 
    gen wgetstr: WINDOW * &char -> int;   // generated
    gen whline: WINDOW * chtype * int -> int;  // implemented 
    gen winch: WINDOW -> chtype;    // implemented 
    gen winchnstr: WINDOW * &chtype * int -> int;  // implemented 
    gen winchstr: WINDOW * &chtype -> int;  // generated
    gen winnstr: WINDOW * &char * int -> int;  // implemented 
    gen winsch: WINDOW * chtype -> int;   // implemented 
    gen winsdelln: WINDOW * int -> int;   // implemented 
    gen winsertln: WINDOW -> int;   // generated
    gen winsnstr: WINDOW * &char * int -> int; // implemented 
    gen winsstr: WINDOW * &char -> int;  // generated
    gen winstr: WINDOW * &char -> int;   // generated
    gen wmove: WINDOW * int * int -> int;   // implemented 
    gen wnoutrefresh: WINDOW -> int;   // implemented 
  //extern NCURSES_EXPORT(int) wprintw (WINDOW * &char * ...)  // implemented 
  //  GCC_PRINTFLIKE(2 * 3);
    proc wprintw: WINDOW * string = '(void)wprintw($1,$2.c_str());';
    gen wredrawln: WINDOW * int * int -> int;  // implemented 
    gen wrefresh: WINDOW -> int;    // implemented 
  //extern NCURSES_EXPORT(int) wscanw (WINDOW * &char * ...) // implemented 
  //  GCC_SCANFLIKE(2 * 3);
    gen wscrl: WINDOW * int -> int;   // implemented 
    gen wsetscrreg: WINDOW * int * int -> int;  // implemented 
    gen wstandout: WINDOW -> int;   // generated
    gen wstandend: WINDOW -> int;   // generated
    gen wsyncdown: WINDOW -> void;   // implemented 
    gen wsyncup: WINDOW -> void;    // implemented 
    gen wtimeout: WINDOW * int -> void;   // implemented 
    gen wtouchln: WINDOW * int * int * int -> int;  // implemented 
    gen wvline: WINDOW * chtype * int -> int;  // implemented 
  
  /*
   * These are also declared in <term.h>:
   */
    gen tigetflag: &char -> int;  // implemented 
    gen tigetnum: &char -> int;  // implemented 
    gen tigetstr: &char -> &char;  // implemented 
    gen putp: &char -> int;    // implemented 
  
  //#if NCURSES_TPARM_VARARGS
  //  gen tparm: &char * ... -> &char; /* &special/
  //#else
  //  gen tparm: &char * long * long * long * long * long * long * long * long * long -> &char; /* &special/
  //  gen tparm_varargs: &char * ... -> &char; /* &special/
  //#endif
  
  /*
   * These functions are not in X/Open * but we use them in macro definitions:
   */
    gen getattrs: WINDOW -> int;   // generated
    gen getcurx: WINDOW -> int;   // generated
    gen getcury: WINDOW -> int;   // generated
    gen getbegx: WINDOW -> int;   // generated
    gen getbegy: WINDOW -> int;   // generated
    gen getmaxx: WINDOW -> int;   // generated
    gen getmaxy: WINDOW -> int;   // generated
    gen getparx: WINDOW -> int;   // generated
    gen getpary: WINDOW -> int;   // generated
  
  /*
   * vid_attr() was implemented originally based on a draft of X/Open curses.
   */
  //#ifndef _XOPEN_SOURCE_EXTENDED
  //#define vid_attr(a * pair * opts) vidattr(a)
  //#endif
  
  /*
   * These functions are extensions - not in X/Open Curses.
   */
  //typedef int (*NCURSES_WINDOW_CB)(WINDOW * address);
  //typedef int (*NCURSES_SCREEN_CB)(&SCREEN * address);
    gen is_term_resized: int * int -> bool;
    gen keybound: int * int -> &char;
    gen curses_version: unit -> &char;
    gen assume_default_colors: int * int -> int;
    gen define_key: &char * int -> int;
    gen key_defined: &char -> int;
    gen keyok: int * bool -> int;
    gen resize_term: int * int -> int;
    gen resizeterm: int * int -> int;
    gen set_escdelay: int -> int;
    gen set_tabsize: int -> int;
    gen use_default_colors: unit -> int;
    gen use_extended_names: bool -> int;
    gen use_legacy_coding: int -> int;
    gen use_screen: SCREEN * NCURSES_SCREEN_CB * address -> int;
    gen use_window: WINDOW * NCURSES_WINDOW_CB * address -> int;
    gen wresize: WINDOW * int * int -> int;
    proc nofilter:1;
  
  /*
   * These extensions provide access to information stored in the WINDOW even
   * when NCURSES_OPAQUE is set:
   */
    gen wgetparent: WINDOW -> WINDOW; // generated
    gen is_cleared: WINDOW -> bool; // generated
    gen is_idcok: WINDOW -> bool;  // generated
    gen is_idlok: WINDOW -> bool;  // generated
    gen is_immedok: WINDOW -> bool; // generated
    gen is_keypad: WINDOW -> bool;  // generated
    gen is_leaveok: WINDOW -> bool; // generated
    gen is_nodelay: WINDOW -> bool; // generated
    gen is_notimeout: WINDOW -> bool; // generated
    gen is_scrollok: WINDOW -> bool; // generated
    gen is_syncok: WINDOW -> bool;  // generated
    gen wgetscrreg: WINDOW * &int * &int -> int; // generated
  
    // Colours
    const 
      COLOR_BLACK,
      COLOR_RED,
      COLOR_GREEN,
      COLOR_YELLOW,
      COLOR_BLUE,
      COLOR_MAGENTA,
      COLOR_CYAN,
      COLOR_WHITE : short
    ;
  
    const A_NORMAL : attr_t;
  
    // Mouse stuff
    type mmask_t = "mmask_t";
    cstruct MEVENT {
      id:short;
      x:int;
      y:int;
      z:int;
      bstate: mmask_t;
    };
  
    const BUTTON1_RELEASED        : mmask_t;
    const BUTTON1_PRESSED         : mmask_t;
    const BUTTON1_CLICKED         : mmask_t;
    const BUTTON1_DOUBLE_CLICKED  : mmask_t;
    const BUTTON1_TRIPLE_CLICKED  : mmask_t;
  
    const BUTTON2_RELEASED        : mmask_t;
    const BUTTON2_PRESSED         : mmask_t;
    const BUTTON2_CLICKED         : mmask_t;
    const BUTTON2_DOUBLE_CLICKED  : mmask_t;
    const BUTTON2_TRIPLE_CLICKED  : mmask_t;
  
    const BUTTON3_RELEASED        : mmask_t;
    const BUTTON3_PRESSED         : mmask_t;
    const BUTTON3_CLICKED         : mmask_t;
    const BUTTON3_DOUBLE_CLICKED  : mmask_t;
    const BUTTON3_TRIPLE_CLICKED  : mmask_t;
  
    const BUTTON4_RELEASED        : mmask_t;
    const BUTTON4_PRESSED         : mmask_t;
    const BUTTON4_CLICKED         : mmask_t;
    const BUTTON4_DOUBLE_CLICKED  : mmask_t;
    const BUTTON4_TRIPLE_CLICKED  : mmask_t;
    const BUTTON_CTRL             : mmask_t;
    const BUTTON_SHIFT            : mmask_t;
    const BUTTON_ALT              : mmask_t;
    const ALL_MOUSE_EVENTS        : mmask_t;
    const REPORT_MOUSE_POSITION   : mmask_t;
  
    gen getmouse: &MEVENT -> int;
    gen ungetmouse: &MEVENT -> int;
    gen mousemask: mmask_t * &mmask_t -> mmask_t;
    gen wenclose: WINDOW * int * int -> bool;
    gen mouseinterval: int -> int;
    gen wmouse_trafo: WINDOW * &int * &int * bool -> bool;
    gen mouse_trafo: &int * &int * bool -> bool;
  
  /* VT100 symbols begin here */
    const ACS_ULCORNER    : char;
    const ACS_LLCORNER    : char;
    const ACS_URCORNER    : char;
    const ACS_LRCORNER    : char;
    const ACS_LTEE        : char;
    const ACS_RTEE        : char;
    const ACS_BTEE        : char;
    const ACS_TTEE        : char;
    const ACS_HLINE       : char;
    const ACS_VLINE       : char;
    const ACS_PLUS        : char;
    const ACS_S1          : char;
    const ACS_S9          : char;
    const ACS_DIAMOND     : char;
    const ACS_CKBOARD     : char;
    const ACS_DEGREE      : char;
    const ACS_PLMINUS     : char;
    const ACS_BULLET      : char;
  /* Teletype 5410v1 symbols begin here */
    const ACS_LARROW      : char;
    const ACS_RARROW      : char;
    const ACS_DARROW      : char;
    const ACS_UARROW      : char;
    const ACS_BOARD       : char;
    const ACS_LANTERN     : char;
    const ACS_BLOCK       : char;
  /*
   * These aren't documented, but a lot of System Vs have them anyway
   * (you can spot pprryyzz{{||}} in a lot of AT&T terminfo strings).
   * The ACS_names may not match AT&T's, our source didn't know them.
   */
    const ACS_S3          : char;
    const ACS_S7          : char;
    const ACS_LEQUAL      : char;
    const ACS_GEQUAL      : char;
    const ACS_PI          : char;
    const ACS_NEQUAL      : char;
    const ACS_STERLING    : char;
  
  /*
   * Line drawing ACS names are of the form ACS_trbl, where t is the top, r
   * is the right, b is the bottom, and l is the left.  t, r, b, and l might
   * be B (blank), S (single), D (double), or T (thick).  The subset defined
   * here only uses B and S.
   */
    const ACS_BSSB        : char;
    const ACS_SSBB        : char;
    const ACS_BBSS        : char;
    const ACS_SBBS        : char;
    const ACS_SBSS        : char;
    const ACS_SSSB        : char;
    const ACS_SSBS        : char;
    const ACS_BSSS        : char;
    const ACS_BSBS        : char;
    const ACS_SBSB        : char;
    const ACS_SSSS        : char;
  /*
   * Pseudo-character tokens outside ASCII range.  The curses wgetch() function
   * will return any given one of these only if the corresponding k- capability
   * is defined in your terminal's terminfo entry.
   *
   * Some keys (KEY_A1, etc) are arranged like this:
   *	a1     up    a3
   *	left   b2    right
   *	c1     down  c3
   *
   * A few key codes do not depend upon the terminfo entry.
   */
  
    const KEY_CODE_YES    : int;
    const KEY_MIN     : int;
    const KEY_BREAK    : int;
    const KEY_SRESET    : int;
    const KEY_RESET    : int;
  /*
   * These definitions were generated by /var/tmp/ncurses.roots/ncurses/ncurses/include/MKkey_defs.sh /var/tmp/ncurses.roots/ncurses/ncurses/include/Caps
   */
    const KEY_DOWN    : int;
    const KEY_UP     : int;
    const KEY_LEFT    : int;
    const KEY_RIGHT    : int;
    const KEY_HOME    : int;
    const KEY_BACKSPACE    : int;
    const KEY_F0     : int;
    const KEY_F1     : int = 'KEY_F(1)';
    const KEY_F2     : int = 'KEY_F(2)';
    const KEY_F3     : int = 'KEY_F(3)';
    const KEY_F4     : int = 'KEY_F(4)';
    const KEY_F5     : int = 'KEY_F(5)';
    const KEY_F6     : int = 'KEY_F(6)';
    const KEY_F7     : int = 'KEY_F(7)';
    const KEY_F8     : int = 'KEY_F(8)';
    const KEY_F9     : int = 'KEY_F(9)';
    const KEY_F10     : int = 'KEY_F(10)';
    const KEY_F11    : int = 'KEY_F(11)';
    const KEY_F12     : int = 'KEY_F(12)';
    const KEY_DL     : int;
    const KEY_IL     : int;
    const KEY_DC     : int;
    const KEY_IC     : int;
    const KEY_EIC     : int;
    const KEY_CLEAR    : int;
    const KEY_EOS     : int;
    const KEY_EOL     : int;
    const KEY_SF     : int;
    const KEY_SR     : int;
    const KEY_NPAGE    : int;
    const KEY_PPAGE    : int;
    const KEY_STAB    : int;
    const KEY_CTAB    : int;
    const KEY_CATAB    : int;
    const KEY_ENTER    : int;
    const KEY_PRINT    : int;
    const KEY_LL     : int;
    const KEY_A1     : int;
    const KEY_A3     : int;
    const KEY_B2     : int;
    const KEY_C1     : int;
    const KEY_C3     : int;
    const KEY_BTAB    : int;
    const KEY_BEG     : int;
    const KEY_CANCEL    : int;
    const KEY_CLOSE    : int;
    const KEY_COMMAND    : int;
    const KEY_COPY    : int;
    const KEY_CREATE    : int;
    const KEY_END     : int;
    const KEY_EXIT    : int;
    const KEY_FIND    : int;
    const KEY_HELP    : int;
    const KEY_MARK    : int;
    const KEY_MESSAGE    : int;
    const KEY_MOVE    : int;
    const KEY_NEXT    : int;
    const KEY_OPEN    : int;
    const KEY_OPTIONS    : int;
    const KEY_PREVIOUS    : int;
    const KEY_REDO    : int;
    const KEY_REFERENCE    : int;
    const KEY_REFRESH    : int;
    const KEY_REPLACE    : int;
    const KEY_RESTART    : int;
    const KEY_RESUME    : int;
    const KEY_SAVE    : int;
    const KEY_SBEG    : int;
    const KEY_SCANCEL    : int;
    const KEY_SCOMMAND    : int;
    const KEY_SCOPY    : int;
    const KEY_SCREATE    : int;
    const KEY_SDC     : int;
    const KEY_SDL     : int;
    const KEY_SELECT    : int;
    const KEY_SEND    : int;
    const KEY_SEOL    : int;
    const KEY_SEXIT    : int;
    const KEY_SFIND    : int;
    const KEY_SHELP    : int;
    const KEY_SHOME    : int;
    const KEY_SIC     : int;
    const KEY_SLEFT    : int;
    const KEY_SMESSAGE    : int;
    const KEY_SMOVE    : int;
    const KEY_SNEXT    : int;
    const KEY_SOPTIONS    : int;
    const KEY_SPREVIOUS    : int;
    const KEY_SPRINT    : int;
    const KEY_SREDO    : int;
    const KEY_SREPLACE    : int;
    const KEY_SRIGHT    : int;
    const KEY_SRSUME    : int;
    const KEY_SSAVE    : int;
    const KEY_SSUSPEND    : int;
    const KEY_SUNDO    : int;
    const KEY_SUSPEND    : int;
    const KEY_UNDO    : int;
    const KEY_MOUSE    : int;
    const KEY_RESIZE    : int;
    const KEY_EVENT    : int;
  
    const KEY_MAX     : int;
  
    fun LINES:1->int = "LINES";
    fun COLS:1->int = "COLS";
  }


Test cases
==========



.. code-block:: felix

  //[ncurses_01.flx]
  include "std/io/ncurses";
  open Ncurses;
  //$ write output to a dummy file descriptor
  var term = FileSystem::pipe();
  var ttype = "vt100";
  var fdo = FileSystem::fdopen_output(term.(0));
  var fdi = FileSystem::fdopen_input(term.(1));
  var s = newterm(ttype,fdi,fdo); // get a screen
  var w = newpad(80,24); // get a window
  wprintw(w,"Hello World !!!"); // Hi	
  var r = refresh();
  delscreen(s);
  assert r == 0;

.. code-block:: felix

  //[ncurses_02.flx]
  include "std/io/ncurses";
  open C_hack;
  open Ncurses;
  //$ write output to a dummy file descriptor
  var w = initscr(); // get a screen
  wprintw(w,"Hello World !!!\nNow Press a key."); // Hi	
  ignore(refresh());
  ignore(wgetch(w));
  ignore(endwin());

.. code-block:: felix

  //[ncurses_03.flx]
  /* Example derived from http://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO */
  include "std/io/ncurses";
  open Ncurses;
  open C_hack;
  
    w := initscr();          // Start curses mode
    ignore(cbreak());        // Line buffering disabled, Pass on
  		           // everty thing to me
    ignore(keypad(w, true)); // I need that nifty F1
  
    var height = 4;
    var width = 8;
    var starty = (LINES() - height) / 2;	// Calculating for a center placement
    var startx = (COLS() - width) / 2;	// of the window
    wprintw(w,"Movement: Arrows, Size: F2 F3 F4 F5, Exit: F1");
    ignore(refresh());
    var my_win = create_newwin(height, width, starty, startx);
    var ch = getch();
    LEFT := int$ord$char$ 'j'; RIGHT := int$ord$char$ 'k';
    UP := int$ord$char$ 'i'; DOWN := int$ord$char$ 'm';
    while not ch == KEY_F1 do
      match ch with
      |$(KEY_LEFT) => 
        destroy_win(my_win); startx = startx - 1;
        my_win = create_newwin(height, width, starty,startx);
      |$(KEY_RIGHT) =>
        destroy_win(my_win); startx = startx + 1;
        my_win = create_newwin(height, width, starty,startx);
      |$(KEY_UP) =>
        destroy_win(my_win); starty = starty - 1;
        my_win = create_newwin(height, width, starty,startx);
      |$(KEY_DOWN) =>
        destroy_win(my_win); starty = starty + 1;
        my_win = create_newwin(height, width, starty,startx);
      |$(KEY_F2) => 
        destroy_win(my_win); width = if width > 2 then width - 1 else width endif;
        my_win = create_newwin(height, width, starty,startx);
      |$(KEY_F3) =>
        destroy_win(my_win); width = if COLS() > width then width + 1 else width endif;
        my_win = create_newwin(height, width, starty,startx);
      |$(KEY_F4) =>
        destroy_win(my_win); height = if height > 2 then height - 1 else height endif;
        my_win = create_newwin(height, width, starty,startx);
      |$(KEY_F5) =>
        destroy_win(my_win); height = if LINES() > height then height + 1 else height endif;
        my_win = create_newwin(height, width, starty,startx);
      |_ => {}();
      endmatch;
      ch = getch();
    done
    ignore(endwin());			/* End curses mode		  */
  
  
  
  fun create_newwin(height:int, width:int, starty:int, startx:int) = {
    local_win := newwin(height, width, starty, startx);
    ignore(box(local_win, 0ui , 0ui));	/* 0, 0 gives default characters 
  					 * for the vertical and horizontal
  					 * lines			*/
    ignore(wrefresh(local_win));		/* Show that box 		*/
    return local_win;
  }
  
  proc destroy_win(local_win:WINDOW) {	
    /* box(local_win, ' ', ' '); : This won't produce the desired
     * result of erasing the window. It will leave it's four corners 
     * and so an ugly remnant of window. 
     */
    var borderch = uint$ord$char$ ' ';
    ignore(wborder(local_win,borderch ,borderch ,borderch ,borderch,
                   borderch,borderch ,borderch ,borderch));
    /* The parameters taken are 
     * 1. win: the window on which to operate
     * 2. ls: character to be used for the left side of the window 
     * 3. rs: character to be used for the right side of the window 
     * 4. ts: character to be used for the top side of the window 
     * 5. bs: character to be used for the bottom side of the window 
     * 6. tl: character to be used for the top left corner of the window 
     * 7. tr: character to be used for the top right corner of the window 
     * 8. bl: character to be used for the bottom left corner of the window 
     * 9. br: character to be used for the bottom right corner of the window
     */
    ignore(wrefresh(local_win));
    ignore(delwin(local_win));
  }
  


.. code-block:: fpc

  //[unix_ncurses.fpc]
  Description: ncurses library
  provides_slib: -lncurses
  provides_dlib: -lncurses
  includes: '"ncurses.h"'


