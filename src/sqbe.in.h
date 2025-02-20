// License information at the end of the file.
//
// Do this:
//     #define SQBE_IMPLEMENTATION
// before you include this file in *one* C/C++ file to create the implementation.

#ifndef SQBE_H_INCLUDED_
#define SQBE_H_INCLUDED_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define SQ_OPAQUE_STRUCT_DEF(x) \
  typedef struct x {            \
    uint32_t u;                 \
  } x

SQ_OPAQUE_STRUCT_DEF(SqLinkage);
SQ_OPAQUE_STRUCT_DEF(SqType);
SQ_OPAQUE_STRUCT_DEF(SqBlock);
SQ_OPAQUE_STRUCT_DEF(SqRef);
SQ_OPAQUE_STRUCT_DEF(SqSymbol);
SQ_OPAQUE_STRUCT_DEF(SqItemCtx);

#undef SQ_OPAQUE_STRUCT_DEF

typedef enum SqTarget {
  SQ_TARGET_DEFAULT,      //
  SQ_TARGET_AMD64_APPLE,  //
  SQ_TARGET_AMD64_SYSV,   //
  SQ_TARGET_AMD64_WIN,    //
  SQ_TARGET_ARM64,        //
  SQ_TARGET_ARM64_APPLE,  //
  SQ_TARGET_RV64,         //
} SqTarget;


typedef int (*SqOutputFn)(const char* fmt, va_list ap);

typedef struct SqConfiguration {
  // SQ_TARGET_DEFAULT for compiling for the current (host) platform, otherwise
  // specify the target to cross-compile to.
  SqTarget target;

  // Where the final assembler is written to.
  FILE* output;

  // debug_flags string can contain the following characters to cause QBE to output
  // information to stderr while compiling. NOTE: no final assembly will be emitted
  // if string other than "" is specified.
  // - P: parsing
  // - M: memory optimization
  // - N: ssa construction
  // - C: copy elimination
  // - F: constant folding
  // - A: abi lowering
  // - I: instruction selection
  // - L: liveness
  // - S: spilling
  // - R: reg. allocation
  // - T: types
  const char* debug_flags;

  // TODO: document, mostly the defaults should be fine though.
  unsigned int max_blocks_per_function;
  unsigned int max_linkage_definitions;
  unsigned int max_compiler_function_reserve;
  unsigned int function_commit_chunk_size;
  unsigned int max_compiler_global_reserve;
  unsigned int global_commit_chunk_size;

  // Customizable output, all error messages, etc. will be vectored through this
  // function. If NULL, goes to stdout.
  SqOutputFn output_function;
} SqConfiguration;

#define SQ_CONFIGURATION_DEFAULT                                \
  ((SqConfiguration){.target = SQ_TARGET_DEFAULT,               \
                     .output = stdout,                          \
                     .debug_flags = "",                         \
                     .max_blocks_per_function = 2048,           \
                     .max_linkage_definitions = 256,            \
                     .max_compiler_function_reserve = 64 << 20, \
                     .function_commit_chunk_size = 64 << 10,    \
                     .max_compiler_global_reserve = 64 << 20,   \
                     .global_commit_chunk_size = 64 << 10,      \
                     .output_function = NULL})

void sq_init(SqConfiguration* config);
bool sq_shutdown(void);

SqLinkage sq_linkage_create(int alignment,
                            bool exported,
                            bool tls,
                            bool common,
                            const char* section_name,
                            const char* section_flags);
#define sq_linkage_default ((SqLinkage){0})
#define sq_linkage_export ((SqLinkage){1})

// These values must match internal Kw, Kl, etc.
typedef enum SqTypeKind {
  SQ_TYPE_W = 0,
  SQ_TYPE_L = 1,
  SQ_TYPE_S = 2,
  SQ_TYPE_D = 3,
  SQ_TYPE_SB = 4,
  SQ_TYPE_UB = 5,
  SQ_TYPE_SH = 6,
  SQ_TYPE_UH = 7,
  SQ_TYPE_C = 8,          // User-defined class
  SQ_TYPE_0 = 9,          // void
  SQ_TYPE_E = -2,         // error
  SQ_TYPE_M = SQ_TYPE_L,  // memory
} SqTypeKind;

#define sq_type_void ((SqType){SQ_TYPE_0})
#define sq_type_word ((SqType){SQ_TYPE_W})
#define sq_type_long ((SqType){SQ_TYPE_L})
#define sq_type_single ((SqType){SQ_TYPE_S})
#define sq_type_double ((SqType){SQ_TYPE_D})
#define sq_type_byte ((SqType){SQ_TYPE_UB})
#define sq_type_half ((SqType){SQ_TYPE_UH})
#define sq_type_sbyte ((SqType){SQ_TYPE_SB})
#define sq_type_shalf ((SqType){SQ_TYPE_SH})
#define sq_type_ubyte ((SqType){SQ_TYPE_UB})
#define sq_type_uhalf ((SqType){SQ_TYPE_UH})

void sq_type_struct_start(const char* name, int align /*=0 for natural*/);
void sq_type_add_field(SqType field);
void sq_type_add_field_with_count(SqType field, uint32_t count);
SqType sq_type_struct_end(void);

void sq_itemctx_activate(SqItemCtx ctx);

// The returned SqItemCtx is already sq_itemctx_activate()d. If generating
// multiple functions or data declarations at the same time, the correct one
// needs to be (re)activated to set the context for subsequent calls to
// sq_data_* functions.
SqItemCtx sq_data_start(SqLinkage linkage, const char* name);
void sq_data_byte(uint8_t val);
void sq_data_half(uint16_t val);
void sq_data_word(uint32_t val);
void sq_data_long(uint64_t val);
void sq_data_string(const char* str);
void sq_data_single(float f);
void sq_data_double(double d);
void sq_data_ref(SqSymbol ref, int64_t offset);
SqSymbol sq_data_end(void);

// The returned SqItemCtx is already sq_itemctx_activate()d. If generating
// multiple functions or data declarations at the same time, the correct one
// needs to be (re)activated to set the context for subsequent calls to
// sq_func_*, sq_ref_*, sq_block_*, sq_i_*, or sq_const_* functions.
SqItemCtx sq_func_start(SqLinkage linkage, SqType return_type, const char* name);
SqSymbol sq_func_end(void);

SqBlock sq_func_get_entry_block(void);

SqRef sq_const_int(int64_t i);
SqRef sq_const_single(float f);
SqRef sq_const_double(double d);

// SqRef are function local, so the return value from cannot be cached across
// functions.
SqRef sq_ref_for_symbol(SqSymbol sym);

// Forward declare a reference for the `_into` versions of instructions.
// Normally this is unnecessary, typically only when using a phi instruction do
// you need the SqRef before the instruction that generates it.
SqRef sq_ref_declare(void);

SqRef sq_ref_extern(const char* name);

#define sq_func_param(type) sq_func_param_named(type, NULL)
SqRef sq_func_param_named(SqType type, const char* name);

#define sq_block_declare() sq_block_declare_named(NULL)
SqBlock sq_block_declare_named(const char* name);

void sq_block_start(SqBlock block);

#define sq_block_declare_and_start() sq_block_declare_and_start_named(NULL)
SqBlock sq_block_declare_and_start_named(const char* name);

void sq_i_ret_void(void);
void sq_i_ret(SqRef val);
void sq_i_jmp(SqBlock block);
void sq_i_jnz(SqRef cond, SqBlock if_true, SqBlock if_false);

// TODO: only 2-branch phi supported currently
SqRef sq_i_phi(SqType size_class, SqBlock block0, SqRef val0, SqBlock block1, SqRef val1);

typedef struct SqCallArg {
  SqType type;
  SqRef value;
} SqCallArg;

#define sq_varargs_begin (SqCallArg){sq_type_void, (SqRef){0}}

SqRef sq_i_calla(SqType result,
                 SqRef func,
                 int num_args,
                 SqCallArg* cas);

SqRef sq_i_call0(SqType result, SqRef func);
SqRef sq_i_call1(SqType result, SqRef func, SqCallArg ca0);
SqRef sq_i_call2(SqType result, SqRef func, SqCallArg ca0, SqCallArg ca1);
SqRef sq_i_call3(SqType result, SqRef func, SqCallArg ca0, SqCallArg ca1, SqCallArg ca2);
SqRef sq_i_call4(SqType result, SqRef func, SqCallArg ca0, SqCallArg ca1, SqCallArg ca2, SqCallArg ca3);
SqRef sq_i_call5(SqType result, SqRef func, SqCallArg ca0, SqCallArg ca1, SqCallArg ca2, SqCallArg ca3, SqCallArg ca4);
SqRef sq_i_call6(SqType result, SqRef func, SqCallArg ca0, SqCallArg ca1, SqCallArg ca2, SqCallArg ca3, SqCallArg ca4, SqCallArg ca5);

%%%INSTRUCTION_DECLARATIONS%%%

#ifdef __cplusplus
}  // extern "C"
#endif

#endif  /* SQBE_H_INCLUDED_ */
