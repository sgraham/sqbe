#include "sqbe.h"

static PState _ps;

// Blk lifetimes are per-func
// This array is ~2M, perhaps sq_init arg to size this?
static Blk _block_arena[8<<10];
static int _num_blocks;

// Lnk lifetimes are sq_init() scoped
static Lnk _linkage_arena[1<<10];
static int _num_linkages;

typedef enum SqInitStatus {
  SQIS_UNINITIALIZED = 0,
  SQIS_INITIALIZED_EMIT_FIN = 1,
  SQIS_INITIALIZED_NO_FIN = 2,
} SqInitStatus;
static SqInitStatus sq_initialized;

static uint _ntyp;
static Typ* _curty;
static int _curty_build_n;
static uint64_t _curty_build_sz;
static int _curty_build_al;

static int _dbg_name_counter;

static Dat _curd;
static Lnk _curd_lnk;

static void _gen_dbg_name(char* into, const  char* prefix) {
  sprintf(into, "%s_%d", prefix ? prefix : "", _dbg_name_counter++);
}

#define SQ_NAMED_IF_DEBUG(into, provided) \
  if (dbg) {                              \
    _gen_dbg_name(into, provided);        \
  }

#define SQ_COUNTOF(a) (sizeof(a)/sizeof(a[0]))
#define SQ_COUNTOFI(a) ((int)(sizeof(a)/sizeof(a[0])))

#ifdef _MSC_VER
#define alloca _alloca
#endif

static void err(char* s, ...) {
  va_list args;
  va_start(args, s);
  fprintf(stderr, "libqbe: ");
  vfprintf(stderr, s, args);
  fprintf(stderr, "\n");
  va_end(args);
  // TODO: setjmp/longjmp w/ clean up
  exit(1);
}

SqLinkage sq_linkage_create(int alignment,
                            bool exported,
                            bool tls,
                            bool common,
                            const char* section_name,
                            const char* section_flags) {
  SQ_ASSERT(_num_linkages < SQ_COUNTOFI(_linkage_arena));
  SqLinkage ret = {_num_linkages++};
  _linkage_arena[ret.u] = (Lnk){
      .export = exported,
      .thread = tls,
      .common = common,
      .align = alignment,
      .sec = (char*)section_name,
      .secf = (char*)section_flags,
  };
  return ret;
}

static Lnk _linkage_to_internal_lnk(SqLinkage linkage) {
  return _linkage_arena[linkage.u];
}

MAKESURE(ref_sizes_match, sizeof(SqRef) == sizeof(Ref));
MAKESURE(ref_has_expected_size, sizeof(uint32_t) == sizeof(Ref));

static Ref _sqref_to_internal_ref(SqRef ref) {
  Ref ret;
  memcpy(&ret, &ref, sizeof(Ref));
  return ret;
}

static SqRef _internal_ref_to_sqref(Ref ref) {
  SqRef ret;
  memcpy(&ret, &ref, sizeof(Ref));
  return ret;
}

static int _sqtype_to_cls_and_ty(SqType type, int* ty) {
  SQ_ASSERT(type.u != SQ_TYPE_C);
  if (type.u > SQ_TYPE_0) {
    *ty = type.u;
    return SQ_TYPE_C;
  } else {
    *ty = Kx;
    return type.u;
  }
}

static Blk* _sqblock_to_internal_blk(SqBlock block) {
  return &_block_arena[block.u];
}

void sq_init(SqTarget target, FILE* output, const char* debug_names) {
  SQ_ASSERT(sq_initialized == SQIS_UNINITIALIZED);

  _dbg_name_counter = 0;

  _num_linkages = 0;
  // These have to match the header for sq_linkage_default/export.
  SqLinkage def = sq_linkage_create(8, false, false, false, NULL, NULL);
  SQ_ASSERT(def.u == 0); (void)def;
  SqLinkage exp = sq_linkage_create(8, true, false, false, NULL, NULL);
  SQ_ASSERT(exp.u == 1); (void)exp;

  (void)qbe_main_dbgfile;  // TODO
  (void)amd64_winabi_rclob;

  switch (target) {
    case SQ_TARGET_AMD64_APPLE:
      T = T_amd64_apple;
      break;
    case SQ_TARGET_AMD64_SYSV:
      T = T_amd64_sysv;
      break;
    case SQ_TARGET_AMD64_WIN:
      T = T_amd64_win;
      break;
    case SQ_TARGET_ARM64:
      T = T_arm64;
      break;
    case SQ_TARGET_ARM64_APPLE:
      T = T_arm64_apple;
      break;
    case SQ_TARGET_RV64:
      T = T_rv64;
      break;
    default: {
#if defined(__APPLE__) && defined(__MACH__)
#  if defined(__aarch64__)
      T = T_arm64_apple;
#  else
      T = T_amd64_apple;
#  endif
#elif defined(_WIN32)
#  if defined(__aarch64__)
#    error port win arm64
#  else
      T = T_amd64_win;
#  endif
#else
#  if defined(__aarch64__)
      T = T_arm64;
#  elif defined(__amd64__) || defined(__amd64) || defined(__x86_64__) || defined(__x86_64)
      T = T_amd64_sysv;
#  elif defined(__riscv)
      T = T_rv64;
#  else
#    error port unknown
#  endif
#endif
    }
  }

  outf = output;

  memset(debug, 0, sizeof(debug));
  for (const char* d = debug_names; *d; ++d) {
    debug[(int)*d] = 1;
  }

  _ntyp = 0;
  typ = vnew(0, sizeof(typ[0]), PHeap);
  // Reserve the ids of the basic types so the .u values can be used as TypeKinds.
  vgrow(&typ, _ntyp + SQ_TYPE_0 + 1);
  _ntyp += SQ_TYPE_0 + 1;

  dbg = debug_names[0] != 0;
  sq_initialized = dbg ? SQIS_INITIALIZED_NO_FIN : SQIS_INITIALIZED_EMIT_FIN;
}

void sq_shutdown(void) {
  SQ_ASSERT(sq_initialized != SQIS_UNINITIALIZED);
  if (sq_initialized == SQIS_INITIALIZED_EMIT_FIN) {
    T.emitfin(outf);
  }
  // TODO: pool flushes, etc
  sq_initialized = SQIS_UNINITIALIZED;
}

SqBlock sq_func_start(SqLinkage linkage, SqType return_type, const char* name) {
  Lnk lnk = _linkage_to_internal_lnk(linkage);
  lnk.align = 16;

  curb = 0;
  _num_blocks = 0;
  curi = insb;
  curf = alloc(sizeof *curf);
  curf->ntmp = 0;
  curf->ncon = 2;
  curf->tmp = vnew(curf->ntmp, sizeof curf->tmp[0], PFn);
  curf->con = vnew(curf->ncon, sizeof curf->con[0], PFn);
  for (int i = 0; i < Tmp0; ++i) {
    if (T.fpr0 <= i && i < T.fpr0 + T.nfpr) {
      newtmp(0, Kd, curf);
    } else {
      newtmp(0, Kl, curf);
    }
  }
  curf->con[0].type = CBits;
  curf->con[0].bits.i = 0xdeaddead; /* UNDEF */
  curf->con[1].type = CBits;
  curf->lnk = lnk;
  curf->leaf = 1;
  blink = &curf->start;
  rcls = _sqtype_to_cls_and_ty(return_type, &curf->retty);
  strncpy(curf->name, name, NString - 1);
  _ps = PLbl;

  return sq_block_declare_and_start();
}

SqRef sq_func_param_named(SqType type, const char* name) {
  int ty;
  int k = _sqtype_to_cls_and_ty(type, &ty);
  Ref r = newtmp(0, Kx, curf);
  SQ_NAMED_IF_DEBUG(curf->tmp[r.val].name, name);
  // TODO: env ptr, varargs
  if (k == Kc) {
    *curi = (Ins){Oparc, Kl, r, {TYPE(ty)}};
  } else if (k >= Ksb) {
    *curi = (Ins){Oparsb + (k - Ksb), Kw, r, {R}};
  } else {
    *curi = (Ins){Opar, k, r, {R}};
  }
  ++curi;
  return _internal_ref_to_sqref(r);
}

SqRef sq_const_int(int64_t i) {
  Con c = {0};
  c.type = CBits;
  c.bits.i = i;
  return _internal_ref_to_sqref(newcon(&c, curf));
}

SqRef sq_const_single(float f) {
  Con c = {0};
  c.type = CBits;
  c.bits.s = f;
  c.flt = 1;
  return _internal_ref_to_sqref(newcon(&c, curf));
}

SqRef sq_const_double(double d) {
  Con c = {0};
  c.type = CBits;
  c.bits.d = d;
  c.flt = 2;
  return _internal_ref_to_sqref(newcon(&c, curf));
}

// This has to return a SqSymbol that's the name that we re-lookup on use rather
// than directly a Ref because Con Refs are stored per function (so when calling
// the function, we need to create a new one in the caller).
SqSymbol sq_func_end(void) {
  if (!curb) {
    err("empty function");
  }
  if (curb->jmp.type == Jxxx) {
    err("last block misses jump");
  }
  curf->mem = vnew(0, sizeof curf->mem[0], PFn);
  curf->nmem = 0;
  curf->nblk = _num_blocks;
  curf->rpo = 0;
  for (Blk* b = curf->start; b; b = b->link) {
    SQ_ASSERT(b->dlink == 0);
  }
  qbe_parse_typecheck(curf);

  SqSymbol ret = {intern(curf->name)};

  qbe_main_func(curf);

  curf = NULL;

  return ret;
}

SqRef sq_ref_for_symbol(SqSymbol sym) {
  SQ_ASSERT(curf);
  Con c = {0};
  c.type = CAddr;
  c.sym.id = sym.u;
  Ref ret = newcon(&c, curf);
  return _internal_ref_to_sqref(ret);
}

SqRef sq_ref_declare(void) {
  Ref tmp = newtmp(NULL, Kx, curf);
  SQ_NAMED_IF_DEBUG(curf->tmp[tmp.val].name, NULL);
  return _internal_ref_to_sqref(tmp);
}

SqRef sq_extern(const char* name) {
  Con c = {0};
  c.type = CAddr;
  c.sym.id = intern((char*)name);
  Ref ret = newcon(&c, curf);
  return _internal_ref_to_sqref(ret);
}

SqBlock sq_block_declare_named(const char* name) {
  SQ_ASSERT(_num_blocks < SQ_COUNTOFI(_block_arena));
  SqBlock ret = {_num_blocks++};
  Blk* blk = _sqblock_to_internal_blk(ret);
  memset(blk, 0, sizeof(Blk));
  blk->id = ret.u;
  SQ_NAMED_IF_DEBUG(blk->name, name);
  return ret;
}

void sq_block_start(SqBlock block) {
  Blk* b = _sqblock_to_internal_blk(block);
  if (curb && curb->jmp.type == Jxxx) {
    qbe_parse_closeblk();
    curb->jmp.type = Jjmp;
    curb->s1 = b;
  }
  if (b->jmp.type != Jxxx) {
    err("multiple definitions of block @%s", b->name);
  }
  *blink = b;
  curb = b;
  plink = &curb->phi;
  _ps = PPhi;
}

SqBlock sq_block_declare_and_start_named(const char* name) {
  SqBlock new = sq_block_declare_named(name);
  sq_block_start(new);
  return new;
}

void sq_i_ret(SqRef val) {
  SQ_ASSERT(_ps == PIns || _ps == PPhi);
  curb->jmp.type = Jretw + rcls;
  if (val.u == 0) {
    curb->jmp.type = Jret0;
  } else if (rcls != K0) {
    Ref r = _sqref_to_internal_ref(val);
    if (req(r, R)) {
      err("invalid return value");
    }
    curb->jmp.arg = r;
  }
  qbe_parse_closeblk();
  _ps = PLbl;
}

void sq_i_ret_void(void) {
  sq_i_ret((SqRef){0});  // TODO: not sure if this is correct == {RTmp, 0}.
}

SqRef sq_i_calla(SqType result,
                 SqRef func,
                 int num_args,
                 SqCallArg* cas) {
  SQ_ASSERT(_ps == PIns || _ps == PPhi);
  // args are inserted into instrs first, then the call
  for (int i = 0; i < num_args; ++i) {
    SQ_ASSERT(curi - insb < NIns);
    int ty;
    int k = _sqtype_to_cls_and_ty(cas[i].type, &ty);
    Ref r = _sqref_to_internal_ref(cas[i].value);
    // TODO: env
    if (k == K0 && req(r, R)) {
      // This is our hacky special case for where '...' would appear in the call.
      *curi = (Ins){.op = Oargv};
    } else if (k == Kc) {
      *curi = (Ins){Oargc, Kl, R, {TYPE(ty), r}};
    } else if (k >= Ksb) {
      *curi = (Ins){Oargsb + (k - Ksb), Kw, R, {r}};
    } else {
      *curi = (Ins){Oarg, k, R, {r}};
    }
    ++curi;
  }

  Ref tmp;

  {
    SQ_ASSERT(curi - insb < NIns);
    int ty;
    int k = _sqtype_to_cls_and_ty(result, &ty);
    curf->leaf = 0;
    *curi = (Ins){0};
    curi->op = Ocall;
    curi->arg[0] = _sqref_to_internal_ref(func);
    if (k == Kc) {
      k = Kl;
      curi->arg[1] = TYPE(ty);
    }
    if (k >= Ksb) {
      k = Kw;
    }
    curi->cls = k;
    tmp = newtmp(NULL, k, curf);
    SQ_NAMED_IF_DEBUG(curf->tmp[tmp.val].name, NULL);
    curi->to = tmp;
    ++curi;
  }
  _ps = PIns;
  return _internal_ref_to_sqref(tmp);
}

SqRef sq_i_call0(SqType result, SqRef func) {
  return sq_i_calla(result, func, 0, NULL);
}

SqRef sq_i_call1(SqType result, SqRef func, SqCallArg ca0) {
  return sq_i_calla(result, func, 1, &ca0);
}

SqRef sq_i_call2(SqType result, SqRef func, SqCallArg ca0, SqCallArg ca1) {
  SqCallArg cas[2] = {ca0, ca1};
  return sq_i_calla(result, func, 2, cas);
}

SqRef sq_i_call3(SqType result, SqRef func, SqCallArg ca0, SqCallArg ca1, SqCallArg ca2) {
  SqCallArg cas[3] = {ca0, ca1, ca2};
  return sq_i_calla(result, func, 3, cas);
}

SqRef sq_i_call4(SqType result,
                 SqRef func,
                 SqCallArg ca0,
                 SqCallArg ca1,
                 SqCallArg ca2,
                 SqCallArg ca3) {
  SqCallArg cas[4] = {ca0, ca1, ca2, ca3};
  return sq_i_calla(result, func, 4, cas);
}

SqRef sq_i_call5(SqType result,
                 SqRef func,
                 SqCallArg ca0,
                 SqCallArg ca1,
                 SqCallArg ca2,
                 SqCallArg ca3,
                 SqCallArg ca4) {
  SqCallArg cas[5] = {ca0, ca1, ca2, ca3, ca4};
  return sq_i_calla(result, func, 5, cas);
}

SqRef sq_i_call6(SqType result,
                 SqRef func,
                 SqCallArg ca0,
                 SqCallArg ca1,
                 SqCallArg ca2,
                 SqCallArg ca3,
                 SqCallArg ca4,
                 SqCallArg ca5) {
  SqCallArg cas[6] = {ca0, ca1, ca2, ca3, ca4, ca5};
  return sq_i_calla(result, func, 6, cas);
}

void sq_i_jmp(SqBlock block) {
  SQ_ASSERT(_ps == PIns || _ps == PPhi);
  curb->jmp.type = Jjmp;
  curb->s1 = _sqblock_to_internal_blk(block);
  qbe_parse_closeblk();
}

void sq_i_jnz(SqRef cond, SqBlock if_true, SqBlock if_false) {
  Ref r = _sqref_to_internal_ref(cond);
  if (req(r, R))
    err("invalid argument for jnz jump");
  curb->jmp.type = Jjnz;
  curb->jmp.arg = r;
  curb->s1 = _sqblock_to_internal_blk(if_true);
  curb->s2 = _sqblock_to_internal_blk(if_false);
  qbe_parse_closeblk();
}

SqRef sq_i_phi(SqType size_class, SqBlock block0, SqRef val0, SqBlock block1, SqRef val1) {
  if (_ps != PPhi || curb == curf->start) {
    err("unexpected phi instruction");
  }

  Ref tmp = newtmp(NULL, Kx, curf);
  SQ_NAMED_IF_DEBUG(curf->tmp[tmp.val].name, NULL);

  Phi* phi = alloc(sizeof *phi);
  phi->to = tmp;
  phi->cls = size_class.u;
  int i = 2;  // TODO: variable if necessary
  phi->arg = vnew(i, sizeof(Ref), PFn);
  phi->arg[0] = _sqref_to_internal_ref(val0);
  phi->arg[1] = _sqref_to_internal_ref(val1);
  phi->blk = vnew(i, sizeof(Blk*), PFn);
  phi->blk[0] = _sqblock_to_internal_blk(block0);
  phi->blk[1] = _sqblock_to_internal_blk(block1);
  phi->narg = i;
  *plink = phi;
  plink = &phi->link;
  _ps = PPhi;
  return _internal_ref_to_sqref(tmp);
}

static void _normal_two_op_instr_into(int op, Ref into, SqType size_class, SqRef arg0, SqRef arg1) {
  SQ_ASSERT(/*size_class.u >= SQ_TYPE_W && */ size_class.u <= SQ_TYPE_D);
  SQ_ASSERT(_ps == PIns || _ps == PPhi);
  SQ_ASSERT(curi - insb < NIns);
  curi->op = op;
  curi->cls = size_class.u;
  curi->to = into;
  curi->arg[0] = _sqref_to_internal_ref(arg0);
  curi->arg[1] = _sqref_to_internal_ref(arg1);
  ++curi;
  _ps = PIns;
}

static SqRef _normal_two_op_instr(int op, SqType size_class, SqRef arg0, SqRef arg1) {
  Ref tmp = newtmp(NULL, Kx, curf);
  SQ_NAMED_IF_DEBUG(curf->tmp[tmp.val].name, NULL);
  _normal_two_op_instr_into(op, tmp, size_class, arg0, arg1);
  return _internal_ref_to_sqref(tmp);
}

static void _normal_two_op_void_instr(int op, SqRef arg0, SqRef arg1) {
  SQ_ASSERT(_ps == PIns || _ps == PPhi);
  SQ_ASSERT(curi - insb < NIns);
  curi->op = op;
  curi->cls = SQ_TYPE_W;
  curi->to = R;
  curi->arg[0] = _sqref_to_internal_ref(arg0);
  curi->arg[1] = _sqref_to_internal_ref(arg1);
  ++curi;
  _ps = PIns;
}

static void _normal_one_op_instr_into(int op, Ref into, SqType size_class, SqRef arg0) {
  SQ_ASSERT(/*size_class.u >= SQ_TYPE_W && */ size_class.u <= SQ_TYPE_D);
  SQ_ASSERT(_ps == PIns || _ps == PPhi);
  SQ_ASSERT(curi - insb < NIns);
  curi->op = op;
  curi->cls = size_class.u;
  curi->to = into;
  curi->arg[0] = _sqref_to_internal_ref(arg0);
  curi->arg[1] = R;
  ++curi;
  _ps = PIns;
}

static SqRef _normal_one_op_instr(int op, SqType size_class, SqRef arg0) {
  Ref tmp = newtmp(NULL, Kx, curf);
  SQ_NAMED_IF_DEBUG(curf->tmp[tmp.val].name, NULL);
  _normal_one_op_instr_into(op, tmp, size_class, arg0);
  return _internal_ref_to_sqref(tmp);
}

void sq_data_start(SqLinkage linkage, const char* name) {
  _curd_lnk = _linkage_to_internal_lnk(linkage);
  if (_curd_lnk.align == 0) {
    _curd_lnk.align = 8;
  }

  _curd = (Dat){0};
  _curd.type = DStart;
  _curd.name = (char*)name;
  _curd.lnk = &_curd_lnk;
  qbe_main_data(&_curd);
}

void sq_data_byte(uint8_t val) {
  _curd.isref = 0;
  _curd.isstr = 0;
  _curd.type = DB;
  _curd.u.num = val;
  qbe_main_data(&_curd);
}

void sq_data_half(uint16_t val) {
  _curd.isref = 0;
  _curd.isstr = 0;
  _curd.type = DH;
  _curd.u.num = val;
  qbe_main_data(&_curd);
}

void sq_data_word(uint32_t val) {
  _curd.isref = 0;
  _curd.isstr = 0;
  _curd.type = DW;
  _curd.u.num = val;
  qbe_main_data(&_curd);
}

void sq_data_long(uint64_t val) {
  _curd.isref = 0;
  _curd.isstr = 0;
  _curd.type = DL;
  _curd.u.num = val;
  qbe_main_data(&_curd);
}

void sq_data_single(float val) {
  _curd.isref = 0;
  _curd.isstr = 0;
  _curd.type = DW;
  _curd.u.flts = val;
  qbe_main_data(&_curd);
}

void sq_data_double(double val) {
  _curd.isref = 0;
  _curd.isstr = 0;
  _curd.type = DL;
  _curd.u.fltd = val;
  qbe_main_data(&_curd);
}

void sq_data_ref(SqSymbol ref, int64_t offset) {
  _curd.isref = 1;
  _curd.isstr = 0;
  _curd.type = DL;
  _curd.u.ref.name = str(ref.u);
  _curd.u.ref.off = offset;
  qbe_main_data(&_curd);
}

static size_t _str_repr(const char* str, char* into) {
  size_t at = 0;

#define EMIT(x)     \
  do {              \
    if (into)       \
      into[at] = x; \
    ++at;           \
  } while (0)

  EMIT('"');

  for (const char* p = str; *p; ++p) {
    switch (*p) {
      case '"':
        EMIT('\\');
        EMIT('"');
        break;
      case '\\':
        EMIT('\\');
        EMIT('"');
        break;
      case '\r':
        EMIT('\\');
        EMIT('r');
        break;
      case '\n':
        EMIT('\\');
        EMIT('n');
        break;
      case '\t':
        EMIT('\\');
        EMIT('t');
        break;
      case '\0':
        EMIT('\\');
        EMIT('0');
        break;
      default:
        EMIT(*p);
        break;
    }
  }

  EMIT('"');
  EMIT(0);

  return at;
}

void sq_data_string(const char* str) {
  _curd.type = DB;
  _curd.isstr = 1;
  // QBE sneakily avoids de-escaping in the tokenizer and re-escaping during
  // emission by just not handling escapes at all and relying on the input
  // format for string escapes being the same as the assembler's. Because we're
  // getting our strings from C here, we need to "repr" it.
  size_t len = _str_repr(str, NULL);
  char* escaped = alloca(len);
  size_t len2 = _str_repr(str, escaped);
  SQ_ASSERT(len == len2);
  (void)len2;
  _curd.u.str = (char*)escaped;
  qbe_main_data(&_curd);
}

SqSymbol sq_data_end(void) {
  _curd.isref = 0;
  _curd.isstr = 0;
  _curd.type = DEnd;
  qbe_main_data(&_curd);

  SqSymbol ret = {intern(_curd.name)};
  _curd = (Dat){0};
  _curd_lnk = (Lnk){0};
  return ret;
}

// Types are a bit questionable (or at least "minimal") in QBE. The specific
// field details are required for determining how to pass at the ABI level
// properly, but in practice, the ABI's only need to know about the first 16 or
// 32 byte of the structure (for example, to determine if the structure should
// be passed in int regs, float regs, or on the stack as a pointer). So, while
// QBE appears to define an arbitrary number of fields, it just drops the
// details of fields beyond the 32nd (but still updates overall struct
// size/alignment for additional values).
void sq_type_struct_start(const char* name, int align) {
  vgrow(&typ, _ntyp + 1);
  _curty = &typ[_ntyp++];
  _curty->isdark = 0;
  _curty->isunion = 0;
  _curty->align = -1;
  _curty_build_n = 0;

  if (align > 0) {
    int al;
    for (al = 0; align /= 2; al++)
      ;
    _curty->align = al;
  }

  _curty->size = 0;
  strcpy(_curty->name, name);
  _curty->fields = vnew(1, sizeof _curty->fields[0], PHeap);
  _curty->nunion = 1;

  _curty_build_sz = 0;
  _curty_build_al = _curty->align;
}

void sq_type_add_field_with_count(SqType field, uint32_t count) {
  Field* fld = _curty->fields[0];

  Typ* ty1;
  uint64_t s;
  int a;

  int type;
  int ty;
  int cls = _sqtype_to_cls_and_ty(field, &ty);
  switch (cls) {
    case SQ_TYPE_D:
      type = Fd;
      s = 8;
      a = 3;
      break;
    case SQ_TYPE_L:
      type = Fl;
      s = 8;
      a = 3;
      break;
    case SQ_TYPE_S:
      type = Fs;
      s = 4;
      a = 2;
      break;
    case SQ_TYPE_W:
      type = Fw;
      s = 4;
      a = 2;
      break;
    case SQ_TYPE_SH:
      type = Fh;
      s = 2;
      a = 1;
      break;
    case SQ_TYPE_UH:
      type = Fh;
      s = 2;
      a = 1;
      break;
    case SQ_TYPE_SB:
      type = Fb;
      s = 1;
      a = 0;
      break;
    case SQ_TYPE_UB:
      type = Fb;
      s = 1;
      a = 0;
      break;
    default:
      type = FTyp;
      ty1 = &typ[field.u];
      s = ty1->size;
      a = ty1->align;
      break;
  }

  if (a > _curty_build_al) {
    _curty_build_al = a;
  }
  a = (1 << a) - 1;
  a = ((_curty_build_sz + a) & ~a) - _curty_build_sz;
  if (a) {
    if (_curty_build_n < NField) {
      fld[_curty_build_n].type = FPad;
      fld[_curty_build_n].len = a;
      _curty_build_n++;
    }
  }
  _curty_build_sz += a + count * s;
  if (type == FTyp) {
    s = field.u;
  }
  for (; count > 0 && _curty_build_n < NField; count--, _curty_build_n++) {
    fld[_curty_build_n].type = type;
    fld[_curty_build_n].len = s;
  }
}

void sq_type_add_field(SqType field) {
  sq_type_add_field_with_count(field, 1);
}

SqType sq_type_struct_end(void) {
  Field* fld = _curty->fields[0];
  fld[_curty_build_n].type = FEnd;
  int a = 1 << _curty_build_al;
  if (_curty_build_sz < _curty->size) {
    _curty_build_sz = _curty->size;
  }
  _curty->size = (_curty_build_sz + a - 1) & -a;
  _curty->align = _curty_build_al;
  if (debug['T']) {
    fprintf(stderr, "\n> Parsed type:\n");
    printtyp(_curty, stderr);
  }
  SqType ret = {_curty - typ};
  _curty = NULL;
  _curty_build_n = 0;
  _curty_build_sz = 0;
  _curty_build_al = 0;
  return ret;
}
