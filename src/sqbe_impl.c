#if defined(_WIN32)
#include <windows.h>
#else
#include <sys/mman.h>
#include <unistd.h>
#endif  // _WIN32

#define SQ_ERR_CHECK(ret) { if (GC(in_error)) return ret; }
#define SQ_ERR_CHECK_VOID() { if (GC(in_error)) return; }

typedef struct SqArena SqArena;

typedef enum SqInitStatus {
  SQIS_UNINITIALIZED = 0,
  SQIS_INITIALIZED_EMIT_FIN = 1,
  SQIS_INITIALIZED_NO_FIN = 2,
} SqInitStatus;

// These things are sqbe state that have to be saved/restored on entry/exit from
// functions (or data)s. Additionally, it has to save/restore a lot of
// GlobalContext (but not all), so we keep a GlobalContext here too.
typedef struct SqPerFuncState {
  // This is allocated for all of them during sq_init, and just reset when done.
  SqArena* fn_arena;

  PState ps;
  Blk* block_pool;  // This is allocated from fn_arena.
  unsigned int num_blocks;
  unsigned int max_blocks;

  Dat curd;
  Lnk curd_lnk;

  SqBlock start_block;

  Ins* insb;
  Ins* curi;
} SqPerFuncState;

typedef struct SqContext {
  SqInitStatus initialized;

  SqArena* global_arena;

  //
  // Global lifetime starts here.
  //
  uint ntyp;  // This is how many elements are in GC(typ), very hokey.
  SqOutputFn output_func;

  Lnk* linkage_pool;  // This is allocated from global_arena.
  unsigned int num_linkages;
  unsigned int max_linkages;

  Typ* curty;
  int curty_build_n;
  uint64_t curty_build_sz;
  int curty_build_al;

  int dbg_name_counter;

  // Copied here during init so allocating a SqPerFuncState has it.
  unsigned int config_max_blocks;

  //
  // Per-func/data lifetime storage.
  //
  SqPerFuncState pfs;  // This is the one that's been sq_itemctx_activate()d,
                       // and is always a copy of something in
                       // saved_per_func_states.
  SqItemCtx current_itemctx;

#define SQ_MAX_FUNC_CONTEXTS 8
  SqPerFuncState saved_per_func_states[SQ_MAX_FUNC_CONTEXTS];
  GlobalContext saved_global_contexts[SQ_MAX_FUNC_CONTEXTS];
  bool saved_per_func_state_in_use[SQ_MAX_FUNC_CONTEXTS];
} SqContext;

#define SQC(x) g_sq_context.x
static SqContext g_sq_context;

static int sq_usermsgf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int ret = SQC(output_func)(fmt, ap);
  va_end(ap);
  return ret;
}

#if defined(_WIN32)

static void* os_mem_reserve(uint64_t size) {
  return VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_NOACCESS);
}

static bool os_mem_commit(void* ptr, uint64_t size) {
  return VirtualAlloc(ptr, size, MEM_COMMIT, PAGE_READWRITE) != 0;
}

static void os_mem_release(void* ptr, uint64_t size) {
  (void)size;
  VirtualFree(ptr, 0, MEM_RELEASE);
}

static uint64_t os_page_size(void) {
  SYSTEM_INFO sysInfo;
  GetSystemInfo(&sysInfo);
  return sysInfo.dwPageSize;
}

#else  // ^^^ _WIN32 / else vvv

static void* os_mem_reserve(uint64_t size) {
  void* result = mmap(0, size, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (result == MAP_FAILED) {
    result = NULL;
  }
  return result;
}

static bool os_mem_commit(void* ptr, uint64_t size) {
  mprotect(ptr, size, PROT_READ|PROT_WRITE);
  return true;
}

static void os_mem_release(void* ptr, uint64_t size) {
  munmap(ptr, size);
}

static uint64_t os_page_size(void) {
  return sysconf(_SC_PAGE_SIZE);
}

#endif

#define SQ_ARENA_HEADER_SIZE 128
struct SqArena {
  uint64_t commit_chunk_size;
  uint64_t cur_pos;
  uint64_t cur_commit;
  uint64_t cur_reserve;
};

#if defined(__clang__)
#  define SQ_BRANCH_EXPECT(expr, val) __builtin_expect((expr), (val))
#else
#  define SQ_BRANCH_EXPECT(expr, val) (expr)
#endif

#if defined(_WIN32)
#  if defined(__SANITIZE_ADDRESS__)
#    define SQ_ASAN_ENABLED 1
#  endif
#elif defined(__clang__)
#  if defined(__has_feature)
#    if __has_feature(address_sanitizer) || defined(__SANITIZE_ADDRESS__)
#      define SQ_ASAN_ENABLED 1
#    endif
#  endif
#endif

#if defined(SQ_ASAN_ENABLED)
void __asan_poison_memory_region(void const volatile *addr, size_t size);
void __asan_unpoison_memory_region(void const volatile *addr, size_t size);
#  define SQ_ASAN_POISON_REGION(addr, size) __asan_poison_memory_region((addr), (size))
#  define SQ_ASAN_UNPOISON_REGION(addr, size) __asan_unpoison_memory_region((addr), (size))
#else
#  define SQ_ASAN_POISON_REGION(addr, size) ((void)(addr), (void)(size))
#  define SQ_ASAN_UNPOISON_REGION(addr, size) ((void)(addr), (void)(size))
#endif

#define SQ_BRANCH_LIKELY(expr) SQ_BRANCH_EXPECT(expr, 1)
#define SQ_BRANCH_UNLIKELY(expr) SQ_BRANCH_EXPECT(expr, 0)
#define SQ_MIN(x, y) ((x) <= (y) ? (x) : (y))
#define SQ_MAX(x, y) ((x) >= (y) ? (x) : (y))
#define SQ_CLAMP_MAX(x, max) SQ_MIN(x, max)
#define SQ_CLAMP_MIN(x, min) SQ_MAX(x, min)
#define SQ_ALIGN_DOWN(n, a) ((n) & ~((a)-1))
#define SQ_ALIGN_UP(n, a) SQ_ALIGN_DOWN((n) + (a)-1, (a))
MAKESURE(sq_arena_struct_too_large, sizeof(SqArena) < SQ_ARENA_HEADER_SIZE);

static SqArena* _sq_arena_create(uint64_t provided_reserve_size, uint64_t provided_commit_size) {
  uint64_t commit_size = ALIGN_UP(provided_commit_size, os_page_size());
  uint64_t reserve_size = ALIGN_UP(provided_reserve_size, os_page_size());

  void* base = os_mem_reserve(reserve_size);
  SQ_ASSERT(base);
  if (!os_mem_commit(base, commit_size)) {
    die("couldn't commit %zu", commit_size);
  }

  SqArena* arena = base;
  arena->commit_chunk_size = commit_size;
  arena->cur_pos = SQ_ARENA_HEADER_SIZE;
  arena->cur_commit = commit_size;
  arena->cur_reserve = reserve_size;

  SQ_ASAN_POISON_REGION(base, commit_size);
  SQ_ASAN_UNPOISON_REGION(base, SQ_ARENA_HEADER_SIZE);

  return arena;
}

static void _sq_arena_destroy(SqArena* arena) {
  os_mem_release(arena, arena->cur_reserve);
}

static void* _sq_arena_push(SqArena* arena, size_t size, size_t align) {
  uint64_t pos_pre = ALIGN_UP(arena->cur_pos, align);
  uint64_t pos_post = pos_pre + size;

  // Extend commit, if necessary.
  if (arena->cur_commit < pos_post) {
    uint64_t commit_post_aligned = pos_post + arena->commit_chunk_size - 1;
    commit_post_aligned -= commit_post_aligned % arena->commit_chunk_size;
    uint64_t commit_post_clamped = SQ_CLAMP_MAX(commit_post_aligned, arena->cur_reserve);
    uint64_t commit_size = commit_post_clamped - arena->cur_commit;
    unsigned char* commit_ptr = (unsigned char*)arena + arena->cur_commit;
    os_mem_commit(commit_ptr, commit_size);
    arena->cur_commit = commit_post_clamped;
  }

  void* result = NULL;
  if (arena->cur_commit >= pos_post) {
    result = (unsigned char*)arena + pos_pre;
    arena->cur_pos = pos_post;
    SQ_ASAN_UNPOISON_REGION(result, size);
  }

  if (SQ_BRANCH_UNLIKELY(result == NULL)) {
    die("allocation failure");
  }

  return result;
}

#if 0
static uint64_t _sq_arena_pos(SqArena* arena) {
  return arena->cur_pos;
}
#endif

static void _sq_arena_pop_to(SqArena* arena, uint64_t pos) {
  uint64_t clamped_pos = SQ_CLAMP_MIN(SQ_ARENA_HEADER_SIZE, pos);

  SQ_ASAN_POISON_REGION((unsigned char*)arena + clamped_pos, arena->cur_pos - clamped_pos);

  arena->cur_pos = clamped_pos;
}

void* emalloc(size_t n) {
  void* p = _sq_arena_push(SQC(global_arena), n, 8);
  memset(p, 0, n);
  return p;
}

void* alloc(size_t n) {
  if (n == 0) {
    return NULL;
  }
  void* p = _sq_arena_push(SQC(pfs.fn_arena), n, 8);
  memset(p, 0, n);
  return p;
}

void freeall(void) {
  _sq_arena_pop_to(SQC(pfs.fn_arena), 0);
}

void qbe_free(void* ptr) {
  (void)ptr;  // Nothing until arena_destroy.
}

static void _sq_gen_dbg_name(char* into, size_t len, const char* prefix) {
  snprintf(into, len, "%s_%d", prefix ? prefix : "", SQC(dbg_name_counter)++);
}

#define SQ_NAMED_IF_DEBUG(into, provided)        \
  if (global_context.main__dbg) {                \
    _sq_gen_dbg_name(into, sizeof(into), provided); \
  }

#define SQ_COUNTOF(a) (sizeof(a) / sizeof(a[0]))
#define SQ_COUNTOFI(a) ((int)(sizeof(a) / sizeof(a[0])))

#ifdef _MSC_VER
#  define alloca _alloca
#endif

static void _clear_initialized_state(void) {
  for (int i = 0; i < SQ_MAX_FUNC_CONTEXTS; ++i) {
    _sq_arena_destroy(g_sq_context.saved_per_func_states[i].fn_arena);
  }
  _sq_arena_destroy(SQC(global_arena));
  memset(&g_sq_context, 0, sizeof(SqContext));

  // Mostly want to clear this on shutdown/error, but save the fact that we're
  // actually in an error. Re-initialization will clear the GlobalContext fully.
  int saved_in_error = GC(in_error);
  memset(&global_context, 0, sizeof(GlobalContext));
  GC(in_error) = saved_in_error;
}

static void err_(char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  SQC(output_func)(fmt, ap);
  va_end(ap);
  sq_usermsgf("\n");
  GC(in_error) = true;
  _clear_initialized_state();
}

static void die_(char* file, char* fmt, ...) {
  sq_usermsgf("%s: internal error: ", file);
  va_list ap;
  va_start(ap, fmt);
  SQC(output_func)(fmt, ap);
  va_end(ap);
  sq_usermsgf("\n");
  SQ_TRAP();
}

SqLinkage sq_linkage_create(int alignment,
                            bool exported,
                            bool tls,
                            bool common,
                            const char* section_name,
                            const char* section_flags) {
  SQ_ERR_CHECK((SqLinkage){0});
  SQ_ASSERT(SQC(num_linkages) < SQC(max_linkages));
  SqLinkage ret = {SQC(num_linkages)++};
  SQC(linkage_pool)[ret.u] = (Lnk){
      .export = exported,
      .thread = tls,
      .common = common,
      .align = alignment,
      .sec = (char*)section_name,
      .secf = (char*)section_flags,
  };
  return ret;
}

static Lnk _sqlinkage_to_internal_lnk(SqLinkage linkage) {
  SQ_ASSERT(linkage.u < SQC(num_linkages));
  return SQC(linkage_pool)[linkage.u];
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

static int _default_output_fn(const char* fmt, va_list ap) {
  int ret = vfprintf(stdout, fmt, ap);
  return ret;
}

static Blk* _sqblock_to_internal_blk(SqBlock block) {
  SQ_ASSERT(block.u < SQC(pfs.num_blocks));
  return &SQC(pfs.block_pool)[block.u];
}

void sq_init(SqConfiguration* config) {
  SQ_ASSERT(SQC(initialized) == SQIS_UNINITIALIZED);

  memset(&g_sq_context, 0, sizeof(SqContext));
  memset(&global_context, 0, sizeof(GlobalContext));

  SQC(current_itemctx).u = SQ_MAX_FUNC_CONTEXTS;  // Hack, mark as none-active.

  SQC(global_arena) =
      _sq_arena_create(config->max_compiler_global_reserve, config->global_commit_chunk_size);
  for (int i = 0; i < SQ_MAX_FUNC_CONTEXTS; ++i) {
    SQC(saved_per_func_states)[i].fn_arena =
        _sq_arena_create(config->max_compiler_function_reserve, config->function_commit_chunk_size);
  }
  SQC(config_max_blocks) = config->max_blocks_per_function;

  SQC(max_linkages) = config->max_linkage_definitions;
  SQC(linkage_pool) =
      _sq_arena_push(SQC(global_arena), sizeof(Lnk) * SQC(max_linkages), _Alignof(Lnk));

  SQC(output_func) = config->output_function ? config->output_function : _default_output_fn;

  // These have to match the header for sq_linkage_default/export.
  SqLinkage def = sq_linkage_create(8, false, false, false, NULL, NULL);
  SQ_ASSERT(def.u == 0);
  (void)def;
  SqLinkage exp = sq_linkage_create(8, true, false, false, NULL, NULL);
  SQ_ASSERT(exp.u == 1);
  (void)exp;

  (void)qbe_main_dbgfile;  // TODO

  switch (config->target) {
    case SQ_TARGET_AMD64_APPLE:
      GC(T) = T_amd64_apple;
      break;
    case SQ_TARGET_AMD64_SYSV:
      GC(T) = T_amd64_sysv;
      break;
    case SQ_TARGET_AMD64_WIN:
      GC(T) = T_amd64_win;
      break;
    case SQ_TARGET_ARM64:
      GC(T) = T_arm64;
      break;
    case SQ_TARGET_ARM64_APPLE:
      GC(T) = T_arm64_apple;
      break;
    case SQ_TARGET_RV64:
      GC(T) = T_rv64;
      break;
    default: {
#if defined(__APPLE__) && defined(__MACH__)
#  if defined(__aarch64__)
      GC(T) = T_arm64_apple;
#  else
      GC(T) = T_amd64_apple;
#  endif
#elif defined(_WIN32)
#  if defined(__aarch64__)
#    error port win arm64
#  else
      GC(T) = T_amd64_win;
#  endif
#else
#  if defined(__aarch64__)
      GC(T) = T_arm64;
#  elif defined(__amd64__) || defined(__amd64) || defined(__x86_64__) || defined(__x86_64)
      GC(T) = T_amd64_sysv;
#  elif defined(__riscv)
      GC(T) = T_rv64;
#  else
#    error port unknown
#  endif
#endif
    }
  }

  global_context.main__outf = config->output;

  memset(GC(debug), 0, sizeof(GC(debug)));
  for (const char* d = config->debug_flags; *d; ++d) {
    GC(debug)[(int)*d] = 1;
  }

  SQC(ntyp) = 0;
  GC(typ) = vnew(0, sizeof(GC(typ)[0]), PHeap);
  // Reserve the ids of the basic types so the .u values can be used as TypeKinds.
  vgrow(&GC(typ), SQC(ntyp) + SQ_TYPE_0 + 1);
  SQC(ntyp) += SQ_TYPE_0 + 1;

  global_context.main__dbg = config->debug_flags[0] != 0;
  SQC(initialized) = global_context.main__dbg ? SQIS_INITIALIZED_NO_FIN : SQIS_INITIALIZED_EMIT_FIN;
}

bool sq_shutdown(void) {
  SQ_ERR_CHECK(false);

  SQ_ASSERT(SQC(initialized) != SQIS_UNINITIALIZED);
  if (SQC(initialized) == SQIS_INITIALIZED_EMIT_FIN) {
    GC(T).emitfin(global_context.main__outf);
  }

  _clear_initialized_state();
  return true;
}

static SqItemCtx _sq_allocate_and_activate_itemctx(void) {
  unsigned int i = 0;
  SqPerFuncState* alloc_pfs = NULL;
  for (i = 0; i < SQ_MAX_FUNC_CONTEXTS; ++i) {
    if (!SQC(saved_per_func_state_in_use)[i]) {
      SQC(saved_per_func_state_in_use)[i] = true;
      alloc_pfs = &SQC(saved_per_func_states)[i];
      break;
    }
  }
  if (i == SQ_MAX_FUNC_CONTEXTS) {
    die("too many func contexts");
  }
  SQ_ASSERT(alloc_pfs);

  _sq_arena_pop_to(alloc_pfs->fn_arena, 0);
  alloc_pfs->ps = 0;
  alloc_pfs->block_pool =
      _sq_arena_push(alloc_pfs->fn_arena, sizeof(Blk) * SQC(config_max_blocks), _Alignof(Blk));
  alloc_pfs->num_blocks = 0;
  alloc_pfs->max_blocks = SQC(config_max_blocks);
  alloc_pfs->curd = (Dat){0};
  alloc_pfs->curd_lnk = (Lnk){0};

  // TODO: probably more that has to be saved/restored from GlobalContext
  alloc_pfs->insb = _sq_arena_push(alloc_pfs->fn_arena, sizeof(Ins) * NIns, _Alignof(Ins));
  alloc_pfs->curi = NULL;

  SqItemCtx ctx = {i};
  sq_itemctx_activate(ctx);
  return ctx;
}

SqItemCtx sq_func_start(SqLinkage linkage, SqType return_type, const char* name) {
  SQ_ERR_CHECK((SqItemCtx){0});

  SqItemCtx ctx = _sq_allocate_and_activate_itemctx();

  Lnk lnk = _sqlinkage_to_internal_lnk(linkage);
  lnk.align = 16;

#define G(x) global_context.parse__##x

  G(curb) = 0;
  SQC(pfs.num_blocks) = 0;
  GC(curi) = GC(insb);
  G(curf) = alloc(sizeof *G(curf));
  G(curf)->ntmp = 0;
  G(curf)->ncon = 2;
  G(curf)->tmp = vnew(G(curf)->ntmp, sizeof G(curf)->tmp[0], PFn);
  G(curf)->con = vnew(G(curf)->ncon, sizeof G(curf)->con[0], PFn);
  for (int i = 0; i < Tmp0; ++i) {
    if (GC(T).fpr0 <= i && i < GC(T).fpr0 + GC(T).nfpr) {
      newtmp(0, Kd, G(curf));
    } else {
      newtmp(0, Kl, G(curf));
    }
  }
  G(curf)->con[0].type = CBits;
  G(curf)->con[0].bits.i = 0xdeaddead; /* UNDEF */
  G(curf)->con[1].type = CBits;
  G(curf)->lnk = lnk;
  G(curf)->leaf = 1;
  G(blink) = &G(curf)->start;
  G(rcls) = _sqtype_to_cls_and_ty(return_type, &G(curf)->retty);
  strncpy(G(curf)->name, name, NString - 1);
  SQC(pfs.ps) = PLbl;

  SQC(pfs.start_block) = sq_block_declare_and_start();
  return ctx;
}

SqBlock sq_func_get_entry_block(void) {
  SQ_ERR_CHECK((SqBlock){0});
  return SQC(pfs.start_block);
}

// TODO: return the old one to help push/pop?
void sq_itemctx_activate(SqItemCtx itemctx) {
  SQ_ERR_CHECK_VOID();

  // "write back" the current state to the saved state
  if (SQC(current_itemctx.u < SQ_MAX_FUNC_CONTEXTS)) {
    unsigned int i = SQC(current_itemctx).u;
    SQC(pfs.insb) = global_context.insb;
    SQC(pfs.curi) = global_context.curi;
    SQC(saved_per_func_states)[i] = SQC(pfs);
  }

  // And then set the current state to the provided state.
  SQ_ASSERT(itemctx.u < SQ_MAX_FUNC_CONTEXTS);
  SQC(pfs) = SQC(saved_per_func_states)[itemctx.u];
  // TODO: more?
  global_context.insb = SQC(pfs.insb);
  global_context.curi = SQC(pfs.curi);
  SQC(current_itemctx) = itemctx;
}

SqRef sq_func_param_named(SqType type, const char* name) {
  SQ_ERR_CHECK((SqRef){0});
  int ty;
  int k = _sqtype_to_cls_and_ty(type, &ty);
  Ref r = newtmp(0, Kx, G(curf));
  SQ_NAMED_IF_DEBUG(G(curf)->tmp[r.val].name, name);
  // TODO: env ptr, varargs
  if (k == Kc) {
    *GC(curi) = (Ins){Oparc, Kl, r, {TYPE(ty)}};
  } else if (k >= Ksb) {
    *GC(curi) = (Ins){Oparsb + (k - Ksb), Kw, r, {NULL_R}};
  } else {
    *GC(curi) = (Ins){Opar, k, r, {NULL_R}};
  }
  ++GC(curi);
  return _internal_ref_to_sqref(r);
}

SqRef sq_const_int(int64_t i) {
  SQ_ERR_CHECK((SqRef){0});
  Con c = {0};
  c.type = CBits;
  c.bits.i = i;
  return _internal_ref_to_sqref(newcon(&c, G(curf)));
}

SqRef sq_const_single(float f) {
  SQ_ERR_CHECK((SqRef){0});
  Con c = {0};
  c.type = CBits;
  c.bits.s = f;
  c.flt = 1;
  return _internal_ref_to_sqref(newcon(&c, G(curf)));
}

SqRef sq_const_double(double d) {
  SQ_ERR_CHECK((SqRef){0});
  Con c = {0};
  c.type = CBits;
  c.bits.d = d;
  c.flt = 2;
  return _internal_ref_to_sqref(newcon(&c, G(curf)));
}

// This has to return a SqSymbol that's the name that we re-lookup on use rather
// than directly a Ref because Con Refs are stored per function (so when calling
// the function, we need to create a new one in the caller).
SqSymbol sq_func_end(void) {
  SQ_ERR_CHECK((SqSymbol){0});
  if (!G(curb)) {
    err_("empty function");
    return (SqSymbol){0};
  }
  if (G(curb)->jmp.type == Jxxx) {
    err_("last block misses jump");
    return (SqSymbol){0};
  }
  G(curf)->mem = vnew(0, sizeof G(curf)->mem[0], PFn);
  G(curf)->nmem = 0;
  G(curf)->nblk = SQC(pfs.num_blocks);
  G(curf)->rpo = 0;
  for (Blk* b = G(curf)->start; b; b = b->link) {
    SQ_ASSERT(b->dlink == 0);
  }
  qbe_parse_typecheck(G(curf));
  if (GC(in_error)) {
    return (SqSymbol){0};
  }

  SqSymbol ret = {intern(G(curf)->name)};

  qbe_main_func(G(curf));

  G(curf) = NULL;

  _sq_arena_pop_to(SQC(pfs.fn_arena), 0);

  return ret;
}

SqRef sq_ref_for_symbol(SqSymbol sym) {
  SQ_ERR_CHECK((SqRef){0});
  SQ_ASSERT(G(curf));
  Con c = {0};
  c.type = CAddr;
  c.sym.id = sym.u;
  Ref ret = newcon(&c, G(curf));
  return _internal_ref_to_sqref(ret);
}

SqRef sq_ref_declare(void) {
  SQ_ERR_CHECK((SqRef){0});
  Ref tmp = newtmp(NULL, Kx, G(curf));
  SQ_NAMED_IF_DEBUG(G(curf)->tmp[tmp.val].name, NULL);
  return _internal_ref_to_sqref(tmp);
}

SqRef sq_ref_extern(const char* name) {
  SQ_ERR_CHECK((SqRef){0});
  Con c = {0};
  c.type = CAddr;
  c.sym.id = intern((char*)name);
  Ref ret = newcon(&c, G(curf));
  return _internal_ref_to_sqref(ret);
}

SqBlock sq_block_declare_named(const char* name) {
  SQ_ERR_CHECK((SqBlock){0});
  SQ_ASSERT(SQC(pfs.num_blocks) < SQC(pfs.max_blocks));
  SqBlock ret = {SQC(pfs.num_blocks)++};
  Blk* blk = _sqblock_to_internal_blk(ret);
  memset(blk, 0, sizeof(Blk));
  blk->id = ret.u;
  SQ_NAMED_IF_DEBUG(blk->name, name);
  return ret;
}

void sq_block_start(SqBlock block) {
  SQ_ERR_CHECK_VOID();
  Blk* b = _sqblock_to_internal_blk(block);
  if (G(curb) && G(curb)->jmp.type == Jxxx) {
    qbe_parse_closeblk();
    G(curb)->jmp.type = Jjmp;
    G(curb)->s1 = b;
  }
  if (b->jmp.type != Jxxx) {
    err("multiple definitions of block @%s", b->name);
  }
  *G(blink) = b;
  G(curb) = b;
  G(plink) = &G(curb)->phi;
  SQC(pfs.ps) = PPhi;
}

SqBlock sq_block_declare_and_start_named(const char* name) {
  SQ_ERR_CHECK((SqBlock){0});
  SqBlock new = sq_block_declare_named(name);
  sq_block_start(new);
  return new;
}

void sq_i_ret(SqRef val) {
  SQ_ERR_CHECK_VOID();
  SQ_ASSERT(SQC(pfs.ps) == PIns || SQC(pfs.ps) == PPhi);
  G(curb)->jmp.type = Jretw + G(rcls);
  if (val.u == 0) {
    G(curb)->jmp.type = Jret0;
  } else if (G(rcls) != K0) {
    Ref r = _sqref_to_internal_ref(val);
    if (req(r, NULL_R)) {
      err("invalid return value");
    }
    G(curb)->jmp.arg = r;
  }
  qbe_parse_closeblk();
  SQC(pfs.ps) = PLbl;
}

void sq_i_ret_void(void) {
  SQ_ERR_CHECK_VOID();
  sq_i_ret((SqRef){0});  // TODO: not sure if this is correct == {RTmp, 0}.
}

SqRef sq_i_calla(SqType result, SqRef func, int num_args, SqCallArg* cas) {
  SQ_ERR_CHECK((SqRef){0});
  SQ_ASSERT(SQC(pfs.ps) == PIns || SQC(pfs.ps) == PPhi);
  // args are inserted into instrs first, then the call
  for (int i = 0; i < num_args; ++i) {
    SQ_ASSERT(GC(curi) - GC(insb) < NIns);
    int ty;
    int k = _sqtype_to_cls_and_ty(cas[i].type, &ty);
    Ref r = _sqref_to_internal_ref(cas[i].value);
    // TODO: env
    if (k == K0 && req(r, NULL_R)) {
      // This is our hacky special case for where '...' would appear in the call.
      *GC(curi) = (Ins){.op = Oargv};
    } else if (k == Kc) {
      *GC(curi) = (Ins){Oargc, Kl, NULL_R, {TYPE(ty), r}};
    } else if (k >= Ksb) {
      *GC(curi) = (Ins){Oargsb + (k - Ksb), Kw, NULL_R, {r}};
    } else {
      *GC(curi) = (Ins){Oarg, k, NULL_R, {r}};
    }
    ++GC(curi);
  }

  Ref tmp;

  {
    SQ_ASSERT(GC(curi) - GC(insb) < NIns);
    int ty;
    int k = _sqtype_to_cls_and_ty(result, &ty);
    G(curf)->leaf = 0;
    *GC(curi) = (Ins){0};
    GC(curi)->op = Ocall;
    GC(curi)->arg[0] = _sqref_to_internal_ref(func);
    if (k == Kc) {
      k = Kl;
      GC(curi)->arg[1] = TYPE(ty);
    }
    if (k >= Ksb) {
      k = Kw;
    }
    GC(curi)->cls = k;
    tmp = newtmp(NULL, k, G(curf));
    SQ_NAMED_IF_DEBUG(G(curf)->tmp[tmp.val].name, NULL);
    GC(curi)->to = tmp;
    ++GC(curi);
  }
  SQC(pfs.ps) = PIns;
  return _internal_ref_to_sqref(tmp);
}

SqRef sq_i_call0(SqType result, SqRef func) {
  SQ_ERR_CHECK((SqRef){0});
  return sq_i_calla(result, func, 0, NULL);
}

SqRef sq_i_call1(SqType result, SqRef func, SqCallArg ca0) {
  SQ_ERR_CHECK((SqRef){0});
  return sq_i_calla(result, func, 1, &ca0);
}

SqRef sq_i_call2(SqType result, SqRef func, SqCallArg ca0, SqCallArg ca1) {
  SQ_ERR_CHECK((SqRef){0});
  SqCallArg cas[2] = {ca0, ca1};
  return sq_i_calla(result, func, 2, cas);
}

SqRef sq_i_call3(SqType result, SqRef func, SqCallArg ca0, SqCallArg ca1, SqCallArg ca2) {
  SQ_ERR_CHECK((SqRef){0});
  SqCallArg cas[3] = {ca0, ca1, ca2};
  return sq_i_calla(result, func, 3, cas);
}

SqRef sq_i_call4(SqType result,
                 SqRef func,
                 SqCallArg ca0,
                 SqCallArg ca1,
                 SqCallArg ca2,
                 SqCallArg ca3) {
  SQ_ERR_CHECK((SqRef){0});
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
  SQ_ERR_CHECK((SqRef){0});
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
  SQ_ERR_CHECK((SqRef){0});
  SqCallArg cas[6] = {ca0, ca1, ca2, ca3, ca4, ca5};
  return sq_i_calla(result, func, 6, cas);
}

void sq_i_jmp(SqBlock block) {
  SQ_ERR_CHECK_VOID();

  SQ_ASSERT(SQC(pfs.ps) == PIns || SQC(pfs.ps) == PPhi);
  G(curb)->jmp.type = Jjmp;
  G(curb)->s1 = _sqblock_to_internal_blk(block);
  qbe_parse_closeblk();
}

void sq_i_jnz(SqRef cond, SqBlock if_true, SqBlock if_false) {
  SQ_ERR_CHECK_VOID();
  Ref r = _sqref_to_internal_ref(cond);
  if (req(r, NULL_R)) {
    err("invalid argument for jnz jump");
  }
  G(curb)->jmp.type = Jjnz;
  G(curb)->jmp.arg = r;
  G(curb)->s1 = _sqblock_to_internal_blk(if_true);
  G(curb)->s2 = _sqblock_to_internal_blk(if_false);
  qbe_parse_closeblk();
}

SqRef sq_i_phi(SqType size_class, SqBlock block0, SqRef val0, SqBlock block1, SqRef val1) {
  SQ_ERR_CHECK((SqRef){0});
  if (SQC(pfs.ps) != PPhi || G(curb) == G(curf)->start) {
    err_("unexpected phi instruction");
    return (SqRef){0};
  }

  Ref tmp = newtmp(NULL, Kx, G(curf));
  SQ_NAMED_IF_DEBUG(G(curf)->tmp[tmp.val].name, NULL);

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
  *G(plink) = phi;
  G(plink) = &phi->link;
  SQC(pfs.ps) = PPhi;
  return _internal_ref_to_sqref(tmp);
}

static void _normal_two_op_instr_into(int op, Ref into, SqType size_class, SqRef arg0, SqRef arg1) {
  SQ_ERR_CHECK_VOID();
  SQ_ASSERT(/*size_class.u >= SQ_TYPE_W && */ size_class.u <= SQ_TYPE_D);
  SQ_ASSERT(SQC(pfs.ps) == PIns || SQC(pfs.ps) == PPhi);
  SQ_ASSERT(GC(curi) - GC(insb) < NIns);
  GC(curi)->op = op;
  GC(curi)->cls = size_class.u;
  GC(curi)->to = into;
  GC(curi)->arg[0] = _sqref_to_internal_ref(arg0);
  GC(curi)->arg[1] = _sqref_to_internal_ref(arg1);
  ++GC(curi);
  SQC(pfs.ps) = PIns;
}

static SqRef _normal_two_op_instr(int op, SqType size_class, SqRef arg0, SqRef arg1) {
  SQ_ERR_CHECK((SqRef){0});
  Ref tmp = newtmp(NULL, Kx, G(curf));
  SQ_NAMED_IF_DEBUG(G(curf)->tmp[tmp.val].name, NULL);
  _normal_two_op_instr_into(op, tmp, size_class, arg0, arg1);
  return _internal_ref_to_sqref(tmp);
}

static void _normal_two_op_void_instr(int op, SqRef arg0, SqRef arg1) {
  SQ_ERR_CHECK_VOID();
  SQ_ASSERT(SQC(pfs.ps) == PIns || SQC(pfs.ps) == PPhi);
  SQ_ASSERT(GC(curi) - GC(insb) < NIns);
  GC(curi)->op = op;
  GC(curi)->cls = SQ_TYPE_W;
  GC(curi)->to = NULL_R;
  GC(curi)->arg[0] = _sqref_to_internal_ref(arg0);
  GC(curi)->arg[1] = _sqref_to_internal_ref(arg1);
  ++GC(curi);
  SQC(pfs.ps) = PIns;
}

static void _normal_one_op_instr_into(int op, Ref into, SqType size_class, SqRef arg0) {
  SQ_ERR_CHECK_VOID();
  SQ_ASSERT(/*size_class.u >= SQ_TYPE_W && */ size_class.u <= SQ_TYPE_D);
  SQ_ASSERT(SQC(pfs.ps) == PIns || SQC(pfs.ps) == PPhi);
  SQ_ASSERT(GC(curi) - GC(insb) < NIns);
  GC(curi)->op = op;
  GC(curi)->cls = size_class.u;
  GC(curi)->to = into;
  GC(curi)->arg[0] = _sqref_to_internal_ref(arg0);
  GC(curi)->arg[1] = NULL_R;
  ++GC(curi);
  SQC(pfs.ps) = PIns;
}

static SqRef _normal_one_op_instr(int op, SqType size_class, SqRef arg0) {
  SQ_ERR_CHECK((SqRef){0});
  Ref tmp = newtmp(NULL, Kx, G(curf));
  SQ_NAMED_IF_DEBUG(G(curf)->tmp[tmp.val].name, NULL);
  _normal_one_op_instr_into(op, tmp, size_class, arg0);
  return _internal_ref_to_sqref(tmp);
}

SqItemCtx sq_data_start(SqLinkage linkage, const char* name) {
  SQ_ERR_CHECK((SqItemCtx){0});

  SqItemCtx ctx = _sq_allocate_and_activate_itemctx();

  SQC(pfs.curd_lnk) = _sqlinkage_to_internal_lnk(linkage);
  if (SQC(pfs.curd_lnk).align == 0) {
    SQC(pfs.curd_lnk).align = 8;
  }

  SQC(pfs.curd) = (Dat){0};
  SQC(pfs.curd).type = DStart;
  SQC(pfs.curd).name = (char*)name;
  SQC(pfs.curd).lnk = &SQC(pfs.curd_lnk);
  qbe_main_data(&SQC(pfs.curd));
  if (GC(in_error)) { return (SqItemCtx){0}; }

  return ctx;
}

void sq_data_byte(uint8_t val) {
  SQ_ERR_CHECK_VOID();
  SQC(pfs.curd).isref = 0;
  SQC(pfs.curd).isstr = 0;
  SQC(pfs.curd).type = DB;
  SQC(pfs.curd).u.num = val;
  qbe_main_data(&SQC(pfs.curd));
  // qbe_main_data need ret_on_err, but below here they're all tailcalls anyway.
}

void sq_data_half(uint16_t val) {
  SQ_ERR_CHECK_VOID();
  SQC(pfs.curd).isref = 0;
  SQC(pfs.curd).isstr = 0;
  SQC(pfs.curd).type = DH;
  SQC(pfs.curd).u.num = val;
  qbe_main_data(&SQC(pfs.curd));
}

void sq_data_word(uint32_t val) {
  SQ_ERR_CHECK_VOID();
  SQC(pfs.curd).isref = 0;
  SQC(pfs.curd).isstr = 0;
  SQC(pfs.curd).type = DW;
  SQC(pfs.curd).u.num = val;
  qbe_main_data(&SQC(pfs.curd));
}

void sq_data_long(uint64_t val) {
  SQ_ERR_CHECK_VOID();
  SQC(pfs.curd).isref = 0;
  SQC(pfs.curd).isstr = 0;
  SQC(pfs.curd).type = DL;
  SQC(pfs.curd).u.num = val;
  qbe_main_data(&SQC(pfs.curd));
}

void sq_data_single(float val) {
  SQ_ERR_CHECK_VOID();
  SQC(pfs.curd).isref = 0;
  SQC(pfs.curd).isstr = 0;
  SQC(pfs.curd).type = DW;
  SQC(pfs.curd).u.flts = val;
  qbe_main_data(&SQC(pfs.curd));
}

void sq_data_double(double val) {
  SQ_ERR_CHECK_VOID();
  SQC(pfs.curd).isref = 0;
  SQC(pfs.curd).isstr = 0;
  SQC(pfs.curd).type = DL;
  SQC(pfs.curd).u.fltd = val;
  qbe_main_data(&SQC(pfs.curd));
}

void sq_data_ref(SqSymbol ref, int64_t offset) {
  SQ_ERR_CHECK_VOID();
  SQC(pfs.curd).isref = 1;
  SQC(pfs.curd).isstr = 0;
  SQC(pfs.curd).type = DL;
  SQC(pfs.curd).u.ref.name = str(ref.u);
  SQC(pfs.curd).u.ref.off = offset;
  qbe_main_data(&SQC(pfs.curd));
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
  SQ_ERR_CHECK_VOID();
  SQC(pfs.curd).type = DB;
  SQC(pfs.curd).isstr = 1;
  // QBE sneakily avoids de-escaping in the tokenizer and re-escaping during
  // emission by just not handling escapes at all and relying on the input
  // format for string escapes being the same as the assembler's. Because we're
  // getting our strings from C here, we need to "repr" it.
  size_t len = _str_repr(str, NULL);
  char* escaped = alloca(len);
  size_t len2 = _str_repr(str, escaped);
  SQ_ASSERT(len == len2);
  (void)len2;
  SQC(pfs.curd).u.str = (char*)escaped;
  qbe_main_data(&SQC(pfs.curd));
}

SqSymbol sq_data_end(void) {
  SQ_ERR_CHECK((SqSymbol){0});
  SQC(pfs.curd).isref = 0;
  SQC(pfs.curd).isstr = 0;
  SQC(pfs.curd).type = DEnd;
  qbe_main_data(&SQC(pfs.curd));

  SqSymbol ret = {intern(SQC(pfs.curd).name)};
  SQC(pfs.curd) = (Dat){0};
  SQC(pfs.curd_lnk) = (Lnk){0};
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
  SQ_ERR_CHECK_VOID();
  vgrow(&GC(typ), SQC(ntyp) + 1);
  SQC(curty) = &GC(typ)[SQC(ntyp)++];
  SQC(curty)->isdark = 0;
  SQC(curty)->isunion = 0;
  SQC(curty)->align = -1;
  SQC(curty_build_n) = 0;

  if (align > 0) {
    int al;
    for (al = 0; align /= 2; al++)
      ;
    SQC(curty)->align = al;
  }

  SQC(curty)->size = 0;
  strcpy(SQC(curty)->name, name);
  SQC(curty)->fields = vnew(1, sizeof SQC(curty)->fields[0], PHeap);
  SQC(curty)->nunion = 1;

  SQC(curty_build_sz) = 0;
  SQC(curty_build_al) = SQC(curty)->align;
}

void sq_type_add_field_with_count(SqType field, uint32_t count) {
  SQ_ERR_CHECK_VOID();
  Field* fld = SQC(curty)->fields[0];

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
      ty1 = &GC(typ)[field.u];
      s = ty1->size;
      a = ty1->align;
      break;
  }

  if (a > SQC(curty_build_al)) {
    SQC(curty_build_al) = a;
  }
  a = (1 << a) - 1;
  a = ((SQC(curty_build_sz) + a) & ~a) - SQC(curty_build_sz);
  if (a) {
    if (SQC(curty_build_n) < NField) {
      fld[SQC(curty_build_n)].type = FPad;
      fld[SQC(curty_build_n)].len = a;
      SQC(curty_build_n)++;
    }
  }
  SQC(curty_build_sz) += a + count * s;
  if (type == FTyp) {
    s = field.u;
  }
  for (; count > 0 && SQC(curty_build_n) < NField; count--, SQC(curty_build_n)++) {
    fld[SQC(curty_build_n)].type = type;
    fld[SQC(curty_build_n)].len = s;
  }
}

void sq_type_add_field(SqType field) {
  SQ_ERR_CHECK_VOID();
  sq_type_add_field_with_count(field, 1);
}

SqType sq_type_struct_end(void) {
  SQ_ERR_CHECK((SqType){0});
  Field* fld = SQC(curty)->fields[0];
  fld[SQC(curty_build_n)].type = FEnd;
  int a = 1 << SQC(curty_build_al);
  if (SQC(curty_build_sz) < SQC(curty)->size) {
    SQC(curty_build_sz) = SQC(curty)->size;
  }
  SQC(curty)->size = (SQC(curty_build_sz) + a - 1) & -a;
  SQC(curty)->align = SQC(curty_build_al);
  if (GC(debug)['T']) {
    fprintf(stderr, "\n> Parsed type:\n");
    printtyp(SQC(curty), stderr);
  }
  SqType ret = {SQC(curty) - GC(typ)};
  SQC(curty) = NULL;
  SQC(curty_build_n) = 0;
  SQC(curty_build_sz) = 0;
  SQC(curty_build_al) = 0;
  return ret;
}
