import os

ROOT_DIR = os.path.normpath(os.path.join(os.path.abspath(os.path.dirname(__file__)), ".."))

import glob
import platform
import re
import string
import subprocess
import sys

SQBE_C_FILES = [
    "all.h",
    "amd64/all.h",
    "arm64/all.h",
    "arm64/apple_shared.h",
    "arm64/emitjit.h",
    "arm64/emitmacho.h",
    "rv64/all.h",
    "abi.c",
    "alias.c",
    "cfg.c",
    "copy.c",
    "emit.c",
    "fold.c",
    "gcm.c",
    "gvn.c",
    "ifopt.c",
    "live.c",
    "load.c",
    "main.c",
    "mem.c",
    "parse.c",
    "rega.c",
    "simpl.c",
    "spill.c",
    "ssa.c",
    "util.c",
    "amd64/emit.c",
    "amd64/isel.c",
    "amd64/sysv.c",
    "amd64/targ.c",
    "amd64/winabi.c",
    "arm64/abi.c",
    "arm64/apple_shared.c",
    "arm64/emit.c",
    "arm64/emitjit.c",
    "arm64/emitmacho.c",
    "arm64/isel.c",
    "arm64/targ.c",
    "rv64/abi.c",
    "rv64/emit.c",
    "rv64/isel.c",
    "rv64/targ.c",
    "../sqbe_impl.c",
]


def namespace_static_funcs(ns, file, contents):
    lines = contents.splitlines()
    function_names = []
    for i, line in enumerate(lines):
        if (i < len(lines) - 1 and line.startswith("static ") and lines[i + 1]
                and not lines[i + 1].startswith("static")
                and lines[i + 1][0] in string.ascii_lowercase):
            fn = lines[i + 1].partition("(")[0]
            if fn == "amd64_memargs" or fn == "arm64_memargs" or fn == "rv64_memargs":
                continue
            function_names.append(fn)
    for fn in function_names:
        contents = re.sub(r"\b" + fn + "\\(", ns + fn + "(", contents)
        contents = re.sub(r"qsort\((.*) " + fn + r"\);", r"qsort(\1 " + ns + fn + ");", contents)
        contents = re.sub(r"loopiter\((.*) " + fn + r"\);", r"loopiter(\1 " + ns + fn + ");",
                          contents)
        contents = contents.replace("desc[0] = (void *)" + fn, "desc[0] = (void *)" + ns + fn)

    return contents


def rename_asserts(contents):
    return contents.replace("assert(", "SQ_ASSERT(")


def emit_renames(ns, contents):
    ns_up = ns.upper()
    for x in ["E", "Ki", "Ka"]:
        contents = re.sub(r"\b" + x + r"\b", ns_up + x, contents)
    for x in ["omap", "rname"]:
        contents = re.sub(r"\b" + x + r"\b", ns + x, contents)
    return contents + "\n#undef CMP\n\n"


def abi_renames(ns, contents):
    ns_up = ns.upper()
    for x in [
            "Cstk",
            "Cptr",
            "Cstk1",
            "Cstk2",
            "Cfpint",
            "Class",
            "AClass",
            "RAlloc",
            "Insl",
            "Params",
            "ArgPassStyle",
            "ArgClass",
            "ExtraAlloc",
            "RegisterUsage",
    ]:
        contents = re.sub(r"\b" + x + r"\b", ns_up + x, contents)
    for x in ["gpreg", "fpreg"]:
        contents = re.sub(r"\b" + x + r"\b", ns + x, contents)
    return contents


def arm64_reg_rename(contents):
    regs = (["R%d" % i for i in range(16)] + ["IP0", "IP1"] + ["R%d" % i for i in range(18, 29)] +
            ["V%d" % i
             for i in range(31)] + ["FP", "LR", "SP", "NFPR", "NGPR", "NGPS", "NFPS", "NCLR"])
    for i in regs:
        contents = re.sub(r"\b%s\b" % i, "QBE_ARM64_%s" % i, contents)
    return contents


def amd64_reg_rename(contents):
    regs = ([
        "RAX",
        "RCX",
        "RDX",
        "RSI",
        "RDI",
        "R8",
        "R9",
        "R10",
        "R11",
        "RBX",
        "R12",
        "R13",
        "R14",
        "R15",
        "RBP",
        "RSP",
    ] + ["XMM%d" % i for i in range(16)] + [
        "NFPR",
        "NGPR",
        "NGPS_SYSV",
        "NGPS_WIN",
        "NFPS",
        "NCLR_SYSV",
        "NCLR_WIN",
        "RGLOB",
    ])
    for i in regs:
        contents = re.sub(r"\b%s\b" % i, "QBE_AMD64_%s" % i, contents)
    return contents


def rv64_reg_rename(contents):
    regs = (["T%d" % i for i in range(7)] +  #
            ["A%d" % i for i in range(8)] +  #
            ["S%d" % i for i in range(12)] +  #
            ["FT%d" % i for i in range(12)] +  #
            ["FA%d" % i for i in range(8)] +  #
            ["FS%d" % i for i in range(12)] +  #
            [  #
                "FP",  #
                "SP",  #
                "GP",  #
                "TP",  #
                "RA",  #
                "NFPR",  #
                "NGPR",  #
                "NGPS",  #
                "NFPS",  #
                "NCLR",  #
                "RGLOB",  #
            ])  #
    for i in regs:
        contents = re.sub(r"\b%s\b" % i, "QBE_RV64_%s" % i, contents)
    return contents


class InstrResults:

    def __init__(self, decls, real_defns, noop_defns):
        self.decls = decls
        self.defns = real_defns
        self.noop_defns = noop_defns


def make_instr_prototypes(ops_h_contents):
    external_only = []
    for x in ops_h_contents.splitlines():
        if "INTERNAL OPERATIONS" in x:
            break
        if x.startswith("O("):
            external_only.append(x)

    def is_no_return(op):
        # Also blit and call, but those are handled specially and aren't in the
        # public section of ops.h.
        return op == "vastart" or op.startswith("store") or op.startswith("dbgloc")

    def is_single_arg_op(type_string_arg_1):
        return type_string_arg_1.count("x") + type_string_arg_1.count("e") == 4

    class_order = [
        "sq_type_word",
        "sq_type_long",
        "sq_type_single",
        "sq_type_double",
    ]

    def only_single_size_class(arg0):
        is_single = arg0.count("e") == 3
        if is_single:
            index_of_non_e = -1
            for i in range(4):
                if arg0[i] != "e":
                    index_of_non_e = i
                    break
            return (True, class_order[index_of_non_e])
        else:
            return (False, None)

    decls = ""
    defns = ""
    noop_defns = ""
    for op in external_only:
        assert op.startswith("O(")
        toks = re.split("[ \t,T()]+", op[2:])
        op = toks[0]
        arg0 = "".join(toks[1:5])
        arg1 = "".join(toks[5:9])
        if is_single_arg_op(arg1):
            # No non-return single arg ops, but some don't need a size_class
            # (because there's only one possibility).
            is_single_size_class, size_class0 = only_single_size_class(arg0)
            if is_single_size_class:
                proto = "SqRef sq_i_%s(SqRef arg0 /*%s*/)" % (op, "".join(arg0))
                proto_i = "void sq_i_%s_into(SqRef into, SqRef arg0 /*%s*/)" % (
                    op,
                    "".join(arg0),
                )
                defns += (proto + " { return _normal_one_op_instr(O%s, %s, arg0); }\n" %
                          (op, size_class0))
                defns += (
                    proto_i +
                    " { _normal_one_op_instr_into(O%s, _sqref_to_internal_ref(into), %s, arg0); }\n"
                    % (op, size_class0))

                noop_defns += "#define sq_i_%s(a0) (SqRef){0}\n" % op
                noop_defns += "#define sq_i_%s_into(into, a0)\n" % op
            else:
                proto = "SqRef sq_i_%s(SqType size_class, SqRef arg0 /*%s*/)" % (
                    op,
                    "".join(arg0),
                )
                proto_i = ("void sq_i_%s_into(SqRef into, SqType size_class, SqRef arg0 /*%s*/)" % (
                    op,
                    "".join(arg0),
                ))
                defns += (proto + " { return _normal_one_op_instr(O%s, size_class, arg0); }\n" %
                          (op))
                defns += (
                    proto_i +
                    " { _normal_one_op_instr_into(O%s, _sqref_to_internal_ref(into), size_class, arg0); }\n"
                    % (op))

                noop_defns += "#define sq_i_%s(s, a0) (SqRef){0}\n" % op
                noop_defns += "#define sq_i_%s_into(into, s, a0)\n" % op
        else:
            if is_no_return(op):
                proto = "void sq_i_%s(SqRef arg0 /*%s*/, SqRef arg1 /*%s*/)" % (
                    op,
                    "".join(arg0),
                    "".join(arg1),
                )
                proto_i = None
                defns += (proto + " { _normal_two_op_void_instr(O%s, arg0, arg1); }\n" % op)

                noop_defns += "#define sq_i_%s(a0, a1)\n" % op
            else:
                # None of these have trivial size classes, only the single op
                # ones have that case.
                proto = ("SqRef sq_i_%s(SqType size_class, SqRef arg0 /*%s*/, SqRef arg1 /*%s*/)" %
                         (op, "".join(arg0), "".join(arg1)))
                proto_i = (
                    "void sq_i_%s_into(SqRef into, SqType size_class, SqRef arg0 /*%s*/, SqRef arg1 /*%s*/)"
                    % (op, "".join(arg0), "".join(arg1)))
                defns += (proto +
                          " { return _normal_two_op_instr(O%s, size_class, arg0, arg1); }\n" % op)
                defns += (
                    proto_i +
                    " { _normal_two_op_instr_into(O%s, _sqref_to_internal_ref(into), size_class, arg0, arg1); }\n"
                    % op)

                noop_defns += "#define sq_i_%s(s, a0, a1) (SqRef){0}\n" % op
                noop_defns += "#define sq_i_%s_into(into, s, a0, a1)\n" % op
        decls += proto + ";\n"
        if proto_i:
            decls += proto_i + ";\n"

    return InstrResults(decls, defns, noop_defns)


def replace_noreturn(contents):
    attr = "__attribute__((noreturn));"
    result = []
    for x in contents.splitlines():
        if x.endswith(attr):
            x = "SQ_NO_RETURN " + x.replace(attr, ";")
        result.append(x)
    return "\n".join(result)


def label_renames(contents):
    # MSVC apparently uses the same namespace for labels and enums.
    # This should probably just be changed upstream.
    contents = contents.replace("goto Ins;", "goto Label_Ins;")
    contents = re.sub(r"\bIns:", "Label_Ins:", contents)
    contents = contents.replace("goto Ref;", "goto Label_Ref;")
    contents = re.sub(r"\bRef:", "Label_Ref:", contents)
    contents = contents.replace("goto Mem;", "goto Label_Mem;")
    contents = re.sub(r"\bMem:", "Label_Mem:", contents)
    contents = contents.replace("goto Jmp;", "goto Label_Jmp;")
    contents = re.sub(r"\bJmp:", "Label_Jmp:", contents)
    return contents


def remove_function(contents, func_ret_type, func_name):
    lines = contents.splitlines()
    result = []
    in_function = False
    for i, x in enumerate(lines):
        if x.startswith(func_name + "(") and lines[i - 1] == func_ret_type:
            result.pop()
            in_function = True
            continue
        elif in_function and x.startswith("}"):
            in_function = False
            continue
        elif in_function:
            continue
        result.append(x)
    return "\n".join(result)


def remove_data(contents, look_for):
    return "\n".join([x for x in contents.splitlines() if not x.startswith(look_for)])


def remove_lines_range(contents, start, end):
    lines = contents.splitlines()
    result = []
    in_removal = False
    for i, x in enumerate(lines):
        if x.startswith(start):
            in_removal = True
            continue
        elif in_removal and x.startswith(end):
            in_removal = False
            continue
        elif in_removal:
            continue
        result.append(x)
    return "\n".join(result)


def fix_missing_static(contents, funcname):
    result = []
    lines = contents.splitlines()
    for i, x in enumerate(lines):
        if x.startswith(funcname + "(") and lines[i - 1] in ("void", "int"):
            result[i - 1] = "static " + result[i - 1]
        result.append(x)
    return "\n".join(result)


def staticize_main_data(contents):
    result = []
    for line in contents.splitlines():
        if line.startswith("extern Target T") or line.startswith("GlobalContext global_context"):
            line = "static " + line.replace("extern ", "")
        result.append(line)
    return "\n".join(result)


def staticize_parse_data(contents):
    result = []
    for line in contents.splitlines():
        if line.startswith("Op optab"):
            line = "static " + line.replace("extern ", "")
        result.append(line)
    return "\n".join(result)


def staticize_targets(contents):
    result = []
    for line in contents.splitlines():
        if line.startswith("Target T_"):
            line = "static " + line
        result.append(line)
    return "\n".join(result)


def staticize_prototypes(contents):
    result = []
    for line in contents.splitlines():
        if line.startswith("void parse(FILE"):
            continue
        if (line.startswith("void ") or line.startswith("uint32_t ") or line.startswith("char *")
                or line.startswith("int ") or line.startswith("uint ") or line.startswith("bits ")
                or line.startswith("Ins *") or line.startswith("Ref ") or
                line.startswith("MachoCtx*") or line.startswith("Blk *") or
                line.startswith("JitCtx*") or line.startswith("void* ")) and line.endswith(");"):
            line = "static " + line
        elif (line.startswith("extern Target T")
              or line.startswith("extern GlobalContext global_context")
              or line.startswith("extern Op ")
              or line.startswith("uint8_t arm64cond")):
            line = "static " + line.replace("extern ", "")
        result.append(line)
    return "\n".join(result)


def write_tail(out, qbe_root):
    out.write("/*\n\nQBE LICENSE:\n\n")
    with open(os.path.join(qbe_root, "LICENSE"), "r") as f:
        license_contents = f.read()
    out.write(license_contents)
    out.write("\n---\n\n")
    out.write("All other sqbe code under the same license,\n"
              "© 2026 Scott Graham <scott.sqbe@h4ck3r.net>\n\n")
    out.write("*/\n\n")


def write_final_header(qbe_root, ops_h_contents, h_contents, instrs):
    with open("sqbe.h", "w", newline="\n") as out:

        out.write(h_contents)

        out.write("\n")
        out.write("// --------------------\n")
        out.write("//    IMPLEMENTATION\n")
        out.write("// --------------------\n")
        out.write("\n")
        out.write("#ifdef SQBE_IMPLEMENTATION\n")
        out.write("#undef SQBE_IMPLEMENTATION\n")

        out.write("""\
#ifdef _MSC_VER
#define SQ_NO_RETURN __declspec(noreturn)
#pragma warning(push)
#pragma warning(disable: 4146)
#pragma warning(disable: 4200)
#pragma warning(disable: 4244)
#pragma warning(disable: 4245)
#pragma warning(disable: 4389)
#pragma warning(disable: 4459)
#pragma warning(disable: 4701)
#else
#define SQ_NO_RETURN __attribute__((noreturn))
#endif

#ifndef SQ_TRAP
#include <stdlib.h>
#define SQ_TRAP() abort()
#endif

#ifndef SQ_ASSERT
#include <assert.h>
#define SQ_ASSERT(x) assert(x)
#endif
""")

        for file in SQBE_C_FILES:
            with open(os.path.join(qbe_root, file), "rb") as f:
                contents = f.read().decode("utf-8")

            ns = "qbe_" + file.replace("/", "_").replace(".c", "") + "_"

            if file.endswith(".c"):
                contents = namespace_static_funcs(ns, file, contents)
                contents = label_renames(contents)
                contents = staticize_targets(contents)

            contents = rename_asserts(contents)

            if file == "arm64/all.h" or file.startswith("arm64/"):
                contents = arm64_reg_rename(contents)

            if file == "amd64/all.h" or file.startswith("amd64/"):
                contents = amd64_reg_rename(contents)

            if file == "rv64/all.h" or file.startswith("rv64/"):
                contents = rv64_reg_rename(contents)

            if file.endswith("/emit.c"):
                contents = emit_renames(ns, contents)

            if file == "emit.c":  # This should be changed upstream.
                contents = fix_missing_static(contents, "emitlnk")

            if file == "cfg.c":  # This should be changed upstream.
                contents = fix_missing_static(contents, "multloop")

            if file == "gvn.c":  # This should be changed upstream.
                contents = fix_missing_static(contents, "cmpeqz")

            if file == "main.c":
                contents = staticize_main_data(contents)

            if file == "parse.c":
                contents = staticize_parse_data(contents)

            if file == "amd64/targ.c":
                contents = contents.replace("Amd64Op amd64_op", "static Amd64Op amd64_op")

            if file == "amd64/sysv.c":
                contents = contents.replace("int amd64_sysv_rsave", "static int amd64_sysv_rsave")
                contents = contents.replace("int amd64_sysv_rclob", "static int amd64_sysv_rclob")

            if file == "amd64/winabi.c":
                contents = contents.replace("int amd64_winabi_rsave",
                                            "static int amd64_winabi_rsave")
                contents = contents.replace("int amd64_winabi_rclob",
                                            "static int amd64_winabi_rclob")

            if file == "arm64/targ.c":
                contents = contents.replace("int arm64_rsave", "static int arm64_rsave")
                contents = contents.replace("int arm64_rclob", "static int arm64_rclob")

            if file == "rv64/targ.c":
                contents = contents.replace("Rv64Op rv64_op", "static Rv64Op rv64_op")
                contents = contents.replace("int rv64_rsave", "static int rv64_rsave")
                contents = contents.replace("int rv64_rclob", "static int rv64_rclob")

            if file.endswith("emitmacho.h"):
                contents = staticize_prototypes(contents)

            if file.endswith("apple_shared.h"):
                contents = staticize_prototypes(contents)

            if file.endswith("emitjit.h"):
                contents = staticize_prototypes(contents)

            if file.endswith("all.h"):
                contents = staticize_prototypes(contents)
                contents = contents.replace(
                    "static void reinit_global_context(GlobalContext* ctx);\n", "")

                # MSVC annoyingly doesn't handle static forward declarations
                # without a size properly and just dies at the point of
                # declaration. We can't easily restructure to get the ops,
                # regcounts, etc. before the decl, so just hardcode and rely on
                # the MAKESUREs to make sure they match.
                contents = contents.replace("extern Amd64Op amd64_op[];",
                                            "static Amd64Op amd64_op[158];")
                contents = contents.replace("extern int amd64_sysv_rsave[];",
                                            "static int amd64_sysv_rsave[25];")
                contents = contents.replace("extern int amd64_sysv_rclob[];",
                                            "static int amd64_sysv_rclob[6];")
                contents = contents.replace(
                    "extern int amd64_winabi_rsave[];",
                    "static int amd64_winabi_rsave[23];",
                )
                contents = contents.replace(
                    "extern int amd64_winabi_rclob[];",
                    "static int amd64_winabi_rclob[8];",
                )
                contents = contents.replace("extern int arm64_rsave[];",
                                            "static int arm64_rsave[44];")
                contents = contents.replace("extern int arm64_rclob[];",
                                            "static int arm64_rclob[19];")
                contents = contents.replace("extern Rv64Op rv64_op[];",
                                            "static Rv64Op rv64_op[158];")
                contents = contents.replace("extern int rv64_rsave[];",
                                            "static int rv64_rsave[34];")
                contents = contents.replace("extern int rv64_rclob[];",
                                            "static int rv64_rclob[24];")

            if (file.endswith("/abi.c") or file.endswith("amd64/sysv.c")
                    or file.endswith("amd64/winabi.c")):
                contents = abi_renames(ns, contents)

            if file == "main.c":
                contents = remove_function(contents, "int", "main")
                contents = remove_lines_range(contents, "static Target *tlist", "};")
                contents = remove_function(contents, "void", "reinit_global_context")

            if file == "arm64/apple_shared.c":
                contents = contents.replace("uint8_t arm64cond",
                                            "static uint8_t arm64cond")

            if file == "util.c":
                contents = remove_function(contents, "void *", "emalloc")
                contents = remove_function(contents, "void *", "alloc")
                contents = remove_function(contents, "void", "freeall")
                contents = remove_function(contents, "void", "qbe_free")
                contents = remove_function(contents, "void", "die_")

            if file == "parse.c":
                contents = remove_function(contents, "void", "parse")
                contents = remove_function(contents, "static void", "qbe_parse_parsedatref")
                contents = remove_function(contents, "static void", "qbe_parse_parsedat")
                contents = remove_function(contents, "static Ref", "qbe_parse_tmpref")
                contents = remove_function(contents, "static void", "qbe_parse_parsetyp")
                contents = remove_function(contents, "static void", "qbe_parse_parsedatstr")
                contents = remove_function(contents, "static void", "qbe_parse_parsefields")
                contents = remove_function(contents, "static void", "qbe_parse_expect")
                contents = remove_function(contents, "static Blk *", "qbe_parse_findblk")
                contents = remove_function(contents, "static PState", "qbe_parse_parseline")
                contents = remove_function(contents, "static int", "qbe_parse_parselnk")
                contents = remove_function(contents, "static int", "qbe_parse_nextnl")
                contents = remove_function(contents, "static int", "qbe_parse_next")
                contents = remove_function(contents, "static int", "qbe_parse_peek")
                contents = remove_function(contents, "static int", "qbe_parse_lex")
                contents = remove_function(contents, "static int64_t", "qbe_parse_getint")
                contents = remove_function(contents, "static int", "qbe_parse_parserefl")
                contents = remove_function(contents, "void", "err_")
                contents = remove_function(contents, "static int", "qbe_parse_findtyp")
                contents = remove_function(contents, "static int", "qbe_parse_parsecls")
                contents = remove_function(contents, "static Ref", "qbe_parse_parseref")
                contents = remove_function(contents, "static Fn *", "qbe_parse_parsefn")
                contents = remove_function(contents, "static void", "qbe_parse_lexinit")
                contents = remove_lines_range(contents, "static struct {", "} tokval;")
                contents = remove_lines_range(contents, "static char *kwmap", "};")
                contents = remove_lines_range(contents, "enum Token {", "};")
                contents = remove_data(contents, "\tNPred =")
                contents = remove_data(contents, "\tTMask =")
                contents = remove_data(contents, "\tK =")
                contents = re.sub(r"enum {[\s]*};", "", contents)

            contents = replace_noreturn(contents)

            out.write("/*** START FILE: %s ***/\n" % file)
            for line in contents.splitlines():
                if line.startswith('#include "all.h"'):
                    out.write("/* skipping all.h */\n")
                    continue
                if line.startswith('#include "emitmacho.h"'):
                    out.write("/* skipping emitmacho.h */\n")
                    continue
                if line.startswith('#include "apple_shared.h"'):
                    out.write("/* skipping apple_shared.h */\n")
                    continue
                if line.startswith('#include "emitjit.h"'):
                    out.write("/* skipping emitjit.h */\n")
                    continue
                if line.startswith('#include "arm64/emitmacho.h"'):
                    out.write("/* skipping arm64/emitmacho.h */\n")
                    continue
                if line.startswith('#include "arm64/emitjit.h"'):
                    out.write("/* skipping arm64/emitjit.h */\n")
                    continue
                if line.startswith('#include "../all.h"'):
                    out.write("/* skipping ../all.h */\n")
                    continue
                if line.startswith('#include "config.h"'):
                    out.write("/* skipping config.h */\n")
                    continue
                if line.startswith("#include <getopt.h>"):
                    out.write("/* skipping getopt.h */\n")
                    continue
                if line.startswith("#include <assert.h>"):
                    out.write("/* skipping assert.h */\n")
                    continue
                if line.startswith("#include <dlfcn.h>"):
                    out.write("/* skipping dlfcn.h */\n")
                    continue
                if line.strip().startswith('#include "ops.h"') or line.strip().startswith(
                        '#include "../ops.h"'):
                    out.write("/* " + 60 * "-" + "including ops.h */\n")
                    out.write(ops_h_contents)
                    out.write("/* " + 60 * "-" + "end of ops.h */\n")
                    continue
                out.write(line)
                out.write("\n")
            out.write("#undef G\n")
            out.write("/*** END FILE: %s ***/\n" % file)

        out.write(instrs.defns)

        out.write("""\

#ifdef _MSC_VER
#pragma warning(pop)
#endif
""")
        out.write("#endif // SQBE_IMPLEMENTATION\n\n")

        write_noop_impls(out, qbe_root, h_contents, instrs)

        write_tail(out, qbe_root)


def write_noop_impls(out, qbe_root, h_contents, instrs):
    out.write("\n")
    out.write("#ifdef SQBE_NOOP\n")
    out.write("#undef SQBE_NOOP\n")
    out.write("\n")

    out.write("// --------------------------\n")
    out.write("//    NO-OP IMPLEMENTATION\n")
    out.write("// --------------------------\n")
    out.write("\n")

    out.write(instrs.noop_defns)

    out.write("#define sq_init(config)\n")
    out.write("#define sq_shutdown() true\n")
    out.write("\n")
    out.write(
        "#define sq_linkage_create(alignment, exported, tls, common, section_name, section_flags) (SqLinkage){0}\n"
    )
    out.write("\n")
    out.write("#define sq_type_struct_start(name, align)\n")
    out.write("#define sq_type_add_field(field)\n")
    out.write("#define sq_type_add_field_with_count(field, count)\n")
    out.write("#define sq_type_struct_end() (SqType){0}\n")
    out.write("\n")
    out.write("#define sq_itemctx_activate(ctx)\n")
    out.write("\n")
    out.write("#define sq_data_start(linkage, name) (SqItemCtx){0}\n")
    out.write("#define sq_data_byte(val)\n")
    out.write("#define sq_data_half(val)\n")
    out.write("#define sq_data_word(val)\n")
    out.write("#define sq_data_long(val)\n")
    out.write("#define sq_data_string(str)\n")
    out.write("#define sq_data_single(f)\n")
    out.write("#define sq_data_double(d)\n")
    out.write("#define sq_data_ref(ref, offset)\n")
    out.write("#define sq_data_end(void) (SqSymbol){0}\n")
    out.write("\n")
    out.write("#define sq_func_start(linkage, return_type, name) (SqItemCtx){0}\n")
    out.write("#define sq_func_end() (SqSymbol){0}\n")
    out.write("\n")
    out.write("#define sq_func_get_entry_block() (SqBlock){0}\n")
    out.write("\n")
    out.write("#define sq_const_int(i) (SqRef){0}\n")
    out.write("#define sq_const_single(f) (SqRef){0}\n")
    out.write("#define sq_const_double(d) (SqRef){0}\n")
    out.write("\n")
    out.write("#define sq_ref_for_symbol(sym) (SqRef){0}\n")
    out.write("\n")
    out.write("#define sq_ref_declare() (SqRef){0}\n")
    out.write("\n")
    out.write("#define sq_ref_extern(name) (SqRef){0}\n")
    out.write("\n")
    out.write("#define sq_func_param_named(type, name) (SqRef){0}\n")
    out.write("\n")
    out.write("#define sq_block_declare_named(name) (SqBlock){0}\n")
    out.write("\n")
    out.write("#define sq_block_start(block)\n")
    out.write("\n")
    out.write("#define sq_block_declare_and_start_named(name) (SqBlock){0}\n")
    out.write("\n")
    out.write("#define sq_i_ret_void();\n")
    out.write("#define sq_i_ret(val);\n")
    out.write("#define sq_i_jmp(block);\n")
    out.write("#define sq_i_jnz(cond, if_true, if_false)\n")
    out.write("\n")
    out.write("#define sq_i_phi(size_class, block0, val0, block1, val1) (SqRef){0}\n")
    out.write("\n")
    out.write("#define sq_i_blit(from, to, num_bytes)\n")
    out.write("\n")
    out.write("#define sq_i_calla(result, func, num_args, cas) (SqRef){0}\n")
    out.write("\n")
    out.write("#define sq_i_call0 sq_i_call_noop\n")
    out.write("#define sq_i_call1 sq_i_call_noop\n")
    out.write("#define sq_i_call2 sq_i_call_noop\n")
    out.write("#define sq_i_call3 sq_i_call_noop\n")
    out.write("#define sq_i_call4 sq_i_call_noop\n")
    out.write("#define sq_i_call5 sq_i_call_noop\n")
    out.write("#define sq_i_call6 sq_i_call_noop\n")
    out.write("static inline SqRef sq_i_call_noop(SqType result, ...) { return (SqRef){0}; }\n")
    out.write("\n")
    out.write("#endif // SQBE_NOOP\n")
    out.write("\n")


def main():
    os.chdir(os.path.join(ROOT_DIR, "src"))

    qbe_root = os.path.join(os.getcwd(), "qbe")
    if not os.path.exists(qbe_root):
        subprocess.check_call(["git", "clone", "git://c9x.me/qbe.git"])

    if len(sys.argv) > 1 and sys.argv[1] == "--fetch":
        subprocess.check_call(["git", "fetch", "origin"], cwd=qbe_root)
    subprocess.check_call(["git", "checkout", "origin/master"], cwd=qbe_root)
    for patch in sorted(glob.glob("patches/*.patch")):
        subprocess.check_call(["git", "am", os.path.join("..", patch)], cwd=qbe_root)

    with open(os.path.join(qbe_root, "ops.h"), "r") as f:
        ops_h_contents = f.read()

    instrs = make_instr_prototypes(ops_h_contents)

    with open("sqbe.in.h", "r") as header_in:
        h_contents = header_in.read()

    h_contents = h_contents.replace("%%%INSTRUCTION_DECLARATIONS%%%\n", instrs.decls)

    write_final_header(qbe_root, ops_h_contents, h_contents, instrs)

    if sys.platform == "win32":
        subprocess.check_call([
            "cl",
            "/D_CRT_SECURE_NO_WARNINGS",
            "/nologo",
            "/W4",
            "/WX",
            "/wd5287",  # TODO!
            "/c",
            "in_c_test.c",
        ])
        subprocess.check_call([
            "cl",
            "/O2",
            "/D_CRT_SECURE_NO_WARNINGS",
            "/nologo",
            "/W4",
            "/WX",
            "/wd5287",  # TODO!
            "/c",
            "in_c_test.c",
        ])
        os.remove("in_c_test.obj")
        print("win32 build ok")
    elif sys.platform == "darwin":
        subprocess.check_call(
            ["clang", "-Wall", "-Wextra", "-Werror", "-pedantic", "-c", "in_c_test.c"])
        subprocess.check_call([
            "clang",
            "-O3",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-pedantic",
            "-c",
            "in_c_test.c",
        ])
        os.remove("in_c_test.o")
        print("darwin build ok")
    elif sys.platform == "linux":
        # Check we can build with gcc and clang
        subprocess.check_call(
            ["gcc", "-Wall", "-Wextra", "-Werror", "-pedantic", "-c", "in_c_test.c"])
        subprocess.check_call([
            "gcc",
            "-O2",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-pedantic",
            "-c",
            "in_c_test.c",
        ])
        subprocess.check_call(
            ["clang", "-Wall", "-Wextra", "-Werror", "-pedantic", "-c", "in_c_test.c"])
        subprocess.check_call([
            "clang",
            "-O3",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-pedantic",
            "-c",
            "in_c_test.c",
            "-o",
            "sqbe.o",
        ])
        # And can link from C++.
        subprocess.check_call([
            "clang++",
            "-Wall",
            "-Wextra",
            "-Werror",
            "sqbe.o",
            "in_cpp_link_test.cc",
            "-o",
            "in_cpp",
        ])
        # And that the the only exported symbols from sqbe are those we expect
        # (prefixed by `sq_`).
        symsp = subprocess.run(["readelf", "-s", "sqbe.o"], capture_output=True)
        os.remove("in_c_test.o")
        os.remove("sqbe.o")
        os.remove("in_cpp")
        syms = str(symsp.stdout, encoding="utf-8").splitlines()
        syms = [l for l in syms if "GLOBAL " in l]
        syms = [l for l in syms if "UND " not in l]
        for s in syms:
            symname = s.split()[-1]
            if not symname.startswith("sq_"):
                print("Unexpected symbol:", symname)
                sys.exit(1)
        print("These are the global exported symbols from sqbe.o. They look ok, but")
        print("confirm that they match the header part of sqbe.h (only).")
        print("-" * 80)
        for s in syms:
            print(s)
        print("-" * 80)

    subprocess.run([sys.executable, os.path.join(ROOT_DIR, "test", "run_tests.py")], check=True)

    print("sqbe.h ready for distribution")


if __name__ == "__main__":
    main()
