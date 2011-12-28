#ifndef _FLX_GUARD_mktutindex
#define _FLX_GUARD_mktutindex
//Input file: src/tools/mktutindex.flx
//Generated by Felix Version 1.1.7dev
//Timestamp: 2011/12/28 21:53:9 UTC
//Timestamp: 2011/12/29 7:53:9 (local)

//FELIX RUNTIME
#include "flx_rtl.hpp"
#include "flx_gc.hpp"
#ifndef FLX_NO_INCLUDES
#include "mktutindex.includes"
#endif


//-----------------------------------------
//USER HEADERS
#include "flx_rtl_config.hpp"
#include <string>
#include <iostream>
#include <cstdlib>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

//-----------------------------------------
//FELIX SYSTEM
namespace flxusr { namespace mktutindex {
struct thread_frame_t;

//-----------------------------------------
//NAME THE TYPES

//PRIMITIVE 5159 INSTANCE 54512: string
typedef ::std::basic_string<char> _a5159t_54512;
typedef int _us2;

//TYPE 54514: string^2
struct _at54514;

//PRIMITIVE 2534 INSTANCE 54515: int
//typedef int int;

//TYPE 54516: int^2
struct _at54516;

//PRIMITIVE 7518 INSTANCE 54517: RE2
typedef ::re2::RE2* _a7518t_54517;

//PRIMITIVE 7517 INSTANCE 54518: RE2_
typedef ::re2::RE2 _a7517t_54518;

//TYPE 54519: bool^2
struct _at54519;

//PRIMITIVE 2527 INSTANCE 54521: address
typedef void * _a2527t_54521;

//PRIMITIVE 10972 INSTANCE 54522: dirent_t
typedef struct dirent* _a10972t_54522;

//PRIMITIVE 10973 INSTANCE 54523: DIR_t
typedef DIR* _a10973t_54523;

//TYPE 54524: string * &DIR_t
struct _tt54524;

//PRIMITIVE 4335 INSTANCE 54525: ostream
typedef ::std::ostream* _a4335t_54525;

//TYPE 54526: ostream * string
struct _tt54526;

//TYPE 54528: DIR_t * dirent_t * &dirent_t * &int
struct _tt54528;

//TYPE 54529: list[string] * string
struct _tt54529;

//TYPE 54530: string * list[string]
struct _tt54530;

//TYPE 54531: string -> void
struct _pt54531;

//PRIMITIVE 4309 INSTANCE 54532: output_text_file
typedef FILE* _a4309t_54532;

//TYPE 54533: output_text_file * string
struct _tt54533;

//PRIMITIVE 4308 INSTANCE 54534: input_text_file
typedef FILE* _a4308t_54534;

//PRIMITIVE 2529 INSTANCE 54535: char
//typedef char char;

//TYPE 54536: char^2
struct _at54536;

//TYPE 54537: string * int
struct _tt54537;

//PRIMITIVE 10831 INSTANCE 54560: stat_t
typedef struct stat _a10831t_54560;

//TYPE 54561: string * &stat_t * &int
struct _tt54561;

//PRIMITIVE 10860 INSTANCE 54562: mode_t
//typedef mode_t mode_t;

//TYPE 54563: mode_t^2
struct _at54563;

//PRIMITIVE 10741 INSTANCE 54565: errno_t
typedef int _a10741t_54565;

//TYPE 54566: errno_t^2
struct _at54566;

//PRIMITIVE 7520 INSTANCE 54567: StringPiece
typedef ::re2::StringPiece _a7520t_54567;

//PRIMITIVE 7583 INSTANCE 54568: Anchor
typedef ::re2::RE2::Anchor _a7583t_54568;

//TYPE 54569: RE2 * StringPiece * int * Anchor * &StringPiece * int
struct _tt54569;

//TYPE 54570: &list[string]^2
struct _at54570;

//TYPE 54571: &list[string] * list[string]
struct _tt54571;


//-----------------------------------------
//DEFINE THE TYPES

//TYPE 54514: string^2
struct _at54514 {
  static size_t const len = 2;
  typedef _a5159t_54512 element_type;
  _a5159t_54512 data[2];
  _at54514() {}
  _at54514(_a5159t_54512 a0, _a5159t_54512 a1) {
    data[0] = a0;
    data[1] = a1;
  }
};

//TYPE 54516: int^2
struct _at54516 {
  static size_t const len = 2;
  typedef int element_type;
  int data[2];
  _at54516() {}
  _at54516(int a0, int a1) {
    data[0] = a0;
    data[1] = a1;
  }
};

//TYPE 54519: bool^2
struct _at54519 {
  static size_t const len = 2;
  typedef _us2 element_type;
  _us2 data[2];
  _at54519() {}
  _at54519(_us2 a0, _us2 a1) {
    data[0] = a0;
    data[1] = a1;
  }
};

//TYPE 54524: string * &DIR_t
struct _tt54524 {
  _a5159t_54512 mem_0;
  _a10973t_54523* mem_1;
  _tt54524(){}
  _tt54524(_a5159t_54512 a0, _a10973t_54523* a1):
    mem_0(a0), mem_1(a1){}
};

//TYPE 54526: ostream * string
struct _tt54526 {
  _a4335t_54525 mem_0;
  _a5159t_54512 mem_1;
  _tt54526(){}
  _tt54526(_a4335t_54525 a0, _a5159t_54512 a1):
    mem_0(a0), mem_1(a1){}
};

//TYPE 54528: DIR_t * dirent_t * &dirent_t * &int
struct _tt54528 {
  _a10973t_54523 mem_0;
  _a10972t_54522 mem_1;
  _a10972t_54522* mem_2;
  int* mem_3;
  _tt54528(){}
  _tt54528(_a10973t_54523 a0, _a10972t_54522 a1, _a10972t_54522* a2, int* a3):
    mem_0(a0), mem_1(a1), mem_2(a2), mem_3(a3){}
};

//TYPE 54529: list[string] * string
struct _tt54529 {
  void* mem_0;
  _a5159t_54512 mem_1;
  _tt54529(){}
  _tt54529(void* a0, _a5159t_54512 a1):
    mem_0(a0), mem_1(a1){}
};

//TYPE 54530: string * list[string]
struct _tt54530 {
  _a5159t_54512 mem_0;
  void* mem_1;
  _tt54530(){}
  _tt54530(_a5159t_54512 a0, void* a1):
    mem_0(a0), mem_1(a1){}
};

//TYPE 54531: string -> void
struct _pt54531: ::flx::rtl::con_t {
  typedef void rettype;
  typedef _a5159t_54512 argtype;
  virtual ::flx::rtl::con_t *call(::flx::rtl::con_t *, _a5159t_54512 const &)=0;
  virtual _pt54531 *clone()=0;
  virtual ::flx::rtl::con_t *resume()=0;
};

//TYPE 54533: output_text_file * string
struct _tt54533 {
  _a4309t_54532 mem_0;
  _a5159t_54512 mem_1;
  _tt54533(){}
  _tt54533(_a4309t_54532 a0, _a5159t_54512 a1):
    mem_0(a0), mem_1(a1){}
};

//TYPE 54536: char^2
struct _at54536 {
  static size_t const len = 2;
  typedef char element_type;
  char data[2];
  _at54536() {}
  _at54536(char a0, char a1) {
    data[0] = a0;
    data[1] = a1;
  }
};

//TYPE 54537: string * int
struct _tt54537 {
  _a5159t_54512 mem_0;
  int mem_1;
  _tt54537(){}
  _tt54537(_a5159t_54512 a0, int a1):
    mem_0(a0), mem_1(a1){}
};

//TYPE 54561: string * &stat_t * &int
struct _tt54561 {
  _a5159t_54512 mem_0;
  _a10831t_54560* mem_1;
  int* mem_2;
  _tt54561(){}
  _tt54561(_a5159t_54512 a0, _a10831t_54560* a1, int* a2):
    mem_0(a0), mem_1(a1), mem_2(a2){}
};

//TYPE 54563: mode_t^2
struct _at54563 {
  static size_t const len = 2;
  typedef mode_t element_type;
  mode_t data[2];
  _at54563() {}
  _at54563(mode_t a0, mode_t a1) {
    data[0] = a0;
    data[1] = a1;
  }
};

//TYPE 54566: errno_t^2
struct _at54566 {
  static size_t const len = 2;
  typedef _a10741t_54565 element_type;
  _a10741t_54565 data[2];
  _at54566() {}
  _at54566(_a10741t_54565 a0, _a10741t_54565 a1) {
    data[0] = a0;
    data[1] = a1;
  }
};

//TYPE 54569: RE2 * StringPiece * int * Anchor * &StringPiece * int
struct _tt54569 {
  _a7518t_54517 mem_0;
  _a7520t_54567 mem_1;
  int mem_2;
  _a7583t_54568 mem_3;
  _a7520t_54567* mem_4;
  int mem_5;
  _tt54569(){}
  _tt54569(_a7518t_54517 a0, _a7520t_54567 a1, int a2, _a7583t_54568 a3, _a7520t_54567* a4, int a5):
    mem_0(a0), mem_1(a1), mem_2(a2), mem_3(a3), mem_4(a4), mem_5(a5){}
};

//TYPE 54570: &list[string]^2
struct _at54570 {
  static size_t const len = 2;
  typedef void** element_type;
  void** data[2];
  _at54570() {}
  _at54570(void** a0, void** a1) {
    data[0] = a0;
    data[1] = a1;
  }
};

//TYPE 54571: &list[string] * list[string]
struct _tt54571 {
  void** mem_0;
  void* mem_1;
  _tt54571(){}
  _tt54571(void** a0, void* a1):
    mem_0(a0), mem_1(a1){}
};


//-----------------------------------------
//DEFINE FUNCTION CLASS NAMES
struct _lam_10497;
struct _init_;
struct rfi;
struct _i21779_f21779__lam_10674__apos_2;
struct _i21813_f21813__lam_10674__apos_2;
struct println;
struct _i44142_f44142__lam_10674__apos_2;


//-----------------------------------------
//DEFINE FUNCTION CLASSES

//------------------------------
//FUNCTION <11034>: _lam_10497
struct _lam_10497: _pt54531 {
  FLX_FMEM_DECL

  int j;
  _us2 found;
  int i;
  int _i20689_v20689_j;
  _us2 _i20688_v20688_found;
  int _i20687_v20687_i;
  _a5159t_54512 _urv19295;
  _a5159t_54512 _urv19968;
  _a5159t_54512 _genout_urv19967;
  _a4308t_54534 _genout_urv19966;
  _a5159t_54512 hline;
  _a4308t_54534 f;
  _a5159t_54512 x;
  _lam_10497(FLX_FPAR_DECL_ONLY);
  _lam_10497* clone();
  ::flx::rtl::con_t *call(::flx::rtl::con_t*,_a5159t_54512 const &);
  ::flx::rtl::con_t *resume();
};

//------------------------------
//FUNCTION <11569>: _init_
struct _init_: ::flx::rtl::con_t {
  FLX_FMEM_DECL

  void* t;
  void* _i44187_v44187_iter__apos_2_mv_1002;
  _pt54531* _f_uncurry;
  void* _i44185_v44185_x;
  void* _i44178_v44178_t;
  void* _i44177_v44177_iter__apos_2_mv_1002;
  _pt54531* _i44176_v44176__f_uncurry;
  void* _i44175_v44175_x;
  void* _i22992_v22992_t;
  void* _i22991_v22991_iter__apos_2_mv_1002;
  _pt54531* _i22990_v22990__f_uncurry;
  int _i22364_v22364_j;
  _us2 _i22363_v22363_found;
  int _i22362_v22362_i;
  int _i22361_v22361_j;
  _us2 _i22360_v22360_found;
  int _i22359_v22359_i;
  _a5159t_54512 _i22358_v22358__urv19295;
  _a5159t_54512 _i22357_v22357__urv19968;
  _a5159t_54512 _i22356_v22356__genout_urv19967;
  _a4308t_54534 _i22355_v22355__genout_urv19966;
  _a5159t_54512 _i22354_v22354_hline;
  _a4308t_54534 _i22353_v22353_f;
  void* _i22330_v22330_t;
  void* _i22329_v22329_iter__apos_2_mv_1002;
  _pt54531* _i22328_v22328__f_uncurry;
  void* _i22327_v22327_t;
  void* _i22326_v22326_iter__apos_2_mv_1002;
  _pt54531* _i22325_v22325__f_uncurry;
  _a5159t_54512 h;
  void* _i22323_v22323_t;
  void* _i22322_v22322_iter__apos_2_mv_1002;
  _a4309t_54532 _genout_urv22294;
  void* _i22290_v22290_t;
  void* _i22289_v22289_iter__apos_2_mv_1002;
  _pt54531* _i22288_v22288__f_uncurry;
  void* _i22220_v22220_t;
  void* _i22219_v22219_iter__apos_2_mv_1002;
  _pt54531* _i22218_v22218__f_uncurry;
  void* _i22217_v22217_t;
  void* _i22216_v22216_iter__apos_2_mv_1002;
  _pt54531* _i22215_v22215__f_uncurry;
  void* _i22213_v22213_t;
  void* _i22212_v22212_iter__apos_2_mv_1002;
  void* acc;
  void* _i21812_v21812_aux__apos_2_mv_1014;
  void* init_uncurry;
  void* _i21810_v21810_x;
  void* _urv16002;
  _a2527t_54521 _genout_urv16070;
  void* files;
  int err;
  _a10972t_54522 eret;
  _a10972t_54522 e;
  _a10973t_54523 d;
  _a5159t_54512 _urv16294;
  void* rfi_mv_10663;
  _a5159t_54512 dname2;
  void* _urv21745;
  _a7518t_54517 _genout_urv21744;
  _a5159t_54512 result;
  _a5159t_54512 _urv12876;
  _init_(FLX_FPAR_DECL_ONLY);
  _init_* clone();
  ::flx::rtl::con_t *call(::flx::rtl::con_t*);
  ::flx::rtl::con_t *resume();
};

//------------------------------
//PROCEDURE <21761>: _init_::rfi
struct rfi {
  FLX_FMEM_DECL
  _init_ *ptr_init_;

  void* _i21781_v21781_acc;
  void* _i21778_v21778_aux__apos_2_mv_1014;
  void* _i21777_v21777_init_uncurry;
  void* _i21776_v21776_x;
  void* _i21775_v21775__urv16002;
  void* _i21774_v21774_files;
  _a2527t_54521 _i21773_v21773__genout_urv16070;
  void* _i21772_v21772_files;
  int _i21771_v21771_err;
  _a10972t_54522 _i21770_v21770_eret;
  _a10972t_54522 _i21769_v21769_e;
  _a10973t_54523 _i21768_v21768_d;
  _a5159t_54512 _i21767_v21767__urv16294;
  void* _i21766_v21766_rfi_mv_10663;
  _us2 rfi_mv_10653;
  _a5159t_54512 _i21762_v21762_dname2;
  rfi  (FLX_FPAR_DECL _init_*);
  void* apply(_a5159t_54512 const &);
};

//------------------------------
//PROCEDURE <21779>: _init_::rfi::_lam_10674'2
struct _i21779_f21779__lam_10674__apos_2 {
  FLX_FMEM_DECL
  rfi *ptrrfi;
  _init_ *ptr_init_;

  void* _i21799_v21799_aux__apos_2_mv_992;
  void* x_uncurry;
  void* y;
  void* _urv13465;
  _us2 _urv13489;
  void* last;
  void* z;
  _us2 _genout_urv13553;
  int _i21791_v21791__lam_10674__apos_2_mv_10699;
  mode_t m;
  int _i21788_v21788_err;
  _a10831t_54560 b;
  _a5159t_54512 _urv13932;
  int _i21785_v21785__lam_10674__apos_2_mv_10692;
  _a5159t_54512 _i21784_v21784_d;
  _a5159t_54512 _i21783_v21783__lam_10674__apos_2_mv_10685;
  _a5159t_54512 _i21780_v21780_f;
  _i21779_f21779__lam_10674__apos_2  (FLX_FPAR_DECL rfi*, _init_*);
  void* apply(_a5159t_54512 const &);
};

//------------------------------
//PROCEDURE <21813>: _init_::_lam_10674'2
struct _i21813_f21813__lam_10674__apos_2 {
  FLX_FMEM_DECL
  _init_ *ptr_init_;

  void* _i44144_v44144_acc;
  void* _i44141_v44141_aux__apos_2_mv_1014;
  void* _i44140_v44140_init_uncurry;
  void* _i44139_v44139_x;
  void* _i44138_v44138__urv16002;
  _a2527t_54521 _i44136_v44136__genout_urv16070;
  void* _i44135_v44135_files;
  int _i44134_v44134_err;
  _a10972t_54522 _i44133_v44133_eret;
  _a10972t_54522 _i44132_v44132_e;
  _a10973t_54523 _i44131_v44131_d;
  _a5159t_54512 _i44130_v44130__urv16294;
  void* _i44129_v44129_rfi_mv_10663;
  _a5159t_54512 _i44127_v44127_dname2;
  void* _i21833_v21833_aux__apos_2_mv_992;
  void* _i21832_v21832_x_uncurry;
  void* _i21831_v21831_y;
  void* _i21830_v21830__urv13465;
  _us2 _i21829_v21829__urv13489;
  void* _i21828_v21828_last;
  void* _i21827_v21827_z;
  _us2 _i21826_v21826__genout_urv13553;
  int _i21825_v21825__lam_10674__apos_2_mv_10699;
  mode_t _i21823_v21823_m;
  int _i21822_v21822_err;
  _a10831t_54560 _i21821_v21821_b;
  _a5159t_54512 _i21820_v21820__urv13932;
  int _i21819_v21819__lam_10674__apos_2_mv_10692;
  _a5159t_54512 _i21818_v21818_d;
  _a5159t_54512 _i21817_v21817__lam_10674__apos_2_mv_10685;
  _a5159t_54512 _i21814_v21814_f;
  _i21813_f21813__lam_10674__apos_2  (FLX_FPAR_DECL _init_*);
  void* apply(_a5159t_54512 const &);
};

//------------------------------
//FUNCTION <41603>: println
struct println: _pt54531 {
  FLX_FMEM_DECL

  _a5159t_54512 _i41632_v41632_x;
  println(FLX_FPAR_DECL_ONLY);
  println* clone();
  ::flx::rtl::con_t *call(::flx::rtl::con_t*,_a5159t_54512 const &);
  ::flx::rtl::con_t *resume();
};

//------------------------------
//PURE C PROCEDURE <41638>: add
void* FLX_REGPARM add(FLX_FPAR_DECL  void*, _a5159t_54512);

//------------------------------
//PROCEDURE <44142>: _init_::_lam_10674'2::_lam_10674'2
struct _i44142_f44142__lam_10674__apos_2 {
  FLX_FMEM_DECL
  _i21813_f21813__lam_10674__apos_2 *ptr_i21813_f21813__lam_10674__apos_2;
  _init_ *ptr_init_;

  void* _i44160_v44160_aux__apos_2_mv_992;
  void* _i44159_v44159_x_uncurry;
  void* _i44158_v44158_y;
  void* _i44157_v44157__urv13465;
  _us2 _i44156_v44156__urv13489;
  void* _i44155_v44155_last;
  void* _i44154_v44154_z;
  _us2 _i44153_v44153__genout_urv13553;
  int _i44152_v44152__lam_10674__apos_2_mv_10699;
  mode_t _i44151_v44151_m;
  int _i44150_v44150_err;
  _a10831t_54560 _i44149_v44149_b;
  _a5159t_54512 _i44148_v44148__urv13932;
  int _i44147_v44147__lam_10674__apos_2_mv_10692;
  _a5159t_54512 _i44146_v44146_d;
  _a5159t_54512 _i44145_v44145__lam_10674__apos_2_mv_10685;
  _a5159t_54512 _i44143_v44143_f;
  _i44142_f44142__lam_10674__apos_2  (FLX_FPAR_DECL _i21813_f21813__lam_10674__apos_2*, _init_*);
  void* apply(_a5159t_54512 const &);
};

struct thread_frame_t {
  int argc;
  char **argv;
  FILE *flx_stdin;
  FILE *flx_stdout;
  FILE *flx_stderr;
  ::flx::gc::generic::gc_profile_t *gcp;
  ::flx::gc::generic::gc_shape_t *shape_list_head;
  thread_frame_t(
  );
  _a5159t_54512 INSTALL_ROOT_TOPDIR;
  _a5159t_54512 sep;
  _a4309t_54532 _i11033_v11033_f;
  void* docs;
  _a7518t_54517 re;

};

FLX_DCL_THREAD_FRAME

}} // namespace flxusr::mktutindex
using namespace ::flxusr::mktutindex;
//header complete
#endif
