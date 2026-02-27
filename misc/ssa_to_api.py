#!/usr/bin/env python3
"""Translate QBE .ssa files to C code using the sqbe API."""

import re
import sys


# ---------------------------------------------------------------------------
# Tokenizer
# ---------------------------------------------------------------------------

class Token:
    __slots__ = ('kind', 'val', 'line')
    def __init__(self, kind, val, line=0):
        self.kind = kind
        self.val = val
        self.line = line
    def __repr__(self):
        return f'Token({self.kind!r}, {self.val!r})'


def tokenize(text):
    tokens = []
    i = 0
    line = 1
    while i < len(text):
        c = text[i]
        if c == '\n':
            line += 1
            i += 1
            continue
        if c in ' \t\r':
            i += 1
            continue
        if c == '#':
            while i < len(text) and text[i] != '\n':
                i += 1
            continue
        if c == '"':
            j = i + 1
            s = ''
            while j < len(text) and text[j] != '"':
                if text[j] == '\\':
                    j += 1
                    if j < len(text):
                        esc = text[j]
                        if esc == 'n': s += '\n'
                        elif esc == 't': s += '\t'
                        elif esc == '\\': s += '\\'
                        elif esc == '"': s += '"'
                        elif esc == '0': s += '\0'
                        else: s += esc
                else:
                    s += text[j]
                j += 1
            tokens.append(Token('STRING', s, line))
            i = j + 1
            continue
        if c in '%$@:':
            j = i + 1
            while j < len(text) and (text[j].isalnum() or text[j] in '_.-'):
                j += 1
            name = text[i+1:j]
            sigil_map = {'%': 'TMP', '$': 'SYM', '@': 'BLK', ':': 'TYP'}
            tokens.append(Token(sigil_map[c], name, line))
            i = j
            continue
        # s_XXXX float literal
        if c == 's' and i+1 < len(text) and text[i+1] == '_':
            j = i + 2
            while j < len(text) and (text[j].isalnum() or text[j] in '_.+-'):
                j += 1
            tokens.append(Token('SFLOAT', text[i+2:j], line))
            i = j
            continue
        # d_XXXX float literal
        if c == 'd' and i+1 < len(text) and text[i+1] == '_':
            j = i + 2
            while j < len(text) and (text[j].isalnum() or text[j] in '_.+-'):
                j += 1
            tokens.append(Token('DFLOAT', text[i+2:j], line))
            i = j
            continue
        # numbers (including negative)
        if c == '-' or c.isdigit():
            j = i + 1 if c == '-' else i
            if c == '-' and (j >= len(text) or not text[j].isdigit()):
                tokens.append(Token('PUNCT', '-', line))
                i = j
                continue
            while j < len(text) and text[j].isdigit():
                j += 1
            if j < len(text) and text[j] == '.':
                j += 1
                while j < len(text) and text[j].isdigit():
                    j += 1
            tokens.append(Token('NUM', text[i:j], line))
            i = j
            continue
        if c in '(){}=,+':
            tokens.append(Token('PUNCT', c, line))
            i += 1
            continue
        if c == '.' and i+2 < len(text) and text[i+1] == '.' and text[i+2] == '.':
            tokens.append(Token('DOTS', '...', line))
            i += 3
            continue
        if c.isalpha() or c == '_':
            j = i
            while j < len(text) and (text[j].isalnum() or text[j] == '_'):
                j += 1
            tokens.append(Token('WORD', text[i:j], line))
            i = j
            continue
        i += 1
    return tokens


# ---------------------------------------------------------------------------
# Name mangling
# ---------------------------------------------------------------------------

def mangle_tmp(name):
    return 'v_' + re.sub(r'[^a-zA-Z0-9_]', '_', name)

def mangle_block(name):
    return 'b_' + re.sub(r'[^a-zA-Z0-9_]', '_', name)

def mangle_sym(name):
    return 'sym_' + re.sub(r'[^a-zA-Z0-9_]', '_', name)


# ---------------------------------------------------------------------------
# Type mapping
# ---------------------------------------------------------------------------

TYPE_MAP = {
    'w': 'sq_type_word', 'l': 'sq_type_long',
    's': 'sq_type_single', 'd': 'sq_type_double',
    'sb': 'sq_type_sbyte', 'ub': 'sq_type_ubyte',
    'sh': 'sq_type_shalf', 'uh': 'sq_type_uhalf',
    'env': 'sq_type_env',
}

# Two-operand: sq_i_OP(type, a0, a1) -> SqRef  (has _into variant)
_TWO_OP_TYPED = {
    'add', 'sub', 'mul', 'div', 'rem', 'udiv', 'urem',
    'and', 'or', 'xor', 'sar', 'shr', 'shl',
    'ceqw', 'cnew', 'csgew', 'csgtw', 'cslew', 'csltw',
    'cugew', 'cugtw', 'culew', 'cultw',
    'ceql', 'cnel', 'csgel', 'csgtl', 'cslel', 'csltl',
    'cugel', 'cugtl', 'culel', 'cultl',
    'ceqs', 'cges', 'cgts', 'cles', 'clts', 'cnes', 'cos', 'cuos',
    'ceqd', 'cged', 'cgtd', 'cled', 'cltd', 'cned', 'cod', 'cuod',
}

# One-operand with type: sq_i_OP(type, a0) -> SqRef  (has _into variant)
_ONE_OP_TYPED = {
    'neg', 'copy',
    'vaarg',
}

# Void one-operand instructions: sq_i_OP(a0)  (no type, no dest)
_VOID_ONE_OP = {
    'vastart',
}

# Void two-operand store instructions: sq_i_OP(val, addr)  (no type, no dest)
_STORE_INSTRS = {
    'storeb', 'storeh', 'storew', 'storel', 'stores', 'stored',
}

# Load instructions: sq_i_OP(type, addr) -> SqRef  (has _into variant)
# loadw/loadl/loads/loadd map to sq_i_load with the corresponding type.
_LOAD_GENERIC = {'loadw': 'sq_type_word', 'loadl': 'sq_type_long',
                 'loads': 'sq_type_single', 'loadd': 'sq_type_double'}
_LOAD_EXT = {
    'load',  # generic: uses assignment type directly
    'loadsb', 'loadub', 'loadsh', 'loaduh', 'loadsw', 'loaduw',
}

# Alloc instructions: sq_i_OP(size) -> SqRef  (no type param, has _into variant)
_ALLOC_INSTRS = {'alloc4', 'alloc8', 'alloc16'}


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------

class ParseError(Exception):
    pass


class Parser:
    def __init__(self, tokens, filename="<stdin>"):
        self.tokens = tokens
        self.pos = 0
        self.filename = filename
        self.lines = []
        self.indent = 1
        self.data_symbols = {}   # name -> C var for SqSymbol
        self.func_symbols = {}   # name -> C var for SqSymbol
        self.type_map = {}       # typename -> C var for SqType
        self.declared_vars = set()  # SqRef names declared in current function

    def peek(self, offset=0):
        p = self.pos + offset
        return self.tokens[p] if p < len(self.tokens) else Token('EOF', '', 0)

    def advance(self):
        t = self.tokens[self.pos] if self.pos < len(self.tokens) else Token('EOF', '', 0)
        self.pos += 1
        return t

    def expect(self, kind, val=None):
        t = self.advance()
        if t.kind != kind or (val is not None and t.val != val):
            raise ParseError(f"{self.filename}:{t.line}: expected {kind} {val!r}, got {t}")
        return t

    def at_end(self):
        return self.pos >= len(self.tokens)

    def emit(self, s):
        self.lines.append('  ' * self.indent + s)

    def emit_raw(self, s):
        self.lines.append(s)

    # -----------------------------------------------------------------------
    # Type parsing
    # -----------------------------------------------------------------------

    def parse_type(self):
        """Try to parse a type. Returns C type string or None."""
        t = self.peek()
        if t.kind == 'WORD' and t.val in TYPE_MAP:
            self.advance()
            return TYPE_MAP[t.val]
        elif t.kind == 'TYP':
            self.advance()
            if t.val in self.type_map:
                return self.type_map[t.val]
            raise ParseError(f"{self.filename}:{t.line}: unknown type :{t.val}")
        return None

    # -----------------------------------------------------------------------
    # Value parsing
    # -----------------------------------------------------------------------

    def parse_val(self):
        """Parse a value: %tmp, $sym, integer, s_/d_ float."""
        t = self.peek()
        if t.kind == 'TMP':
            self.advance()
            return mangle_tmp(t.val)
        elif t.kind == 'SYM':
            self.advance()
            return self._resolve_sym(t.val)
        elif t.kind == 'NUM':
            self.advance()
            return f'sq_const_int({t.val})'
        elif t.kind == 'SFLOAT':
            self.advance()
            return self._sfloat(t.val)
        elif t.kind == 'DFLOAT':
            self.advance()
            return self._dfloat(t.val)
        else:
            raise ParseError(f"{self.filename}:{t.line}: expected value, got {t}")

    def _resolve_sym(self, name):
        if name in self.data_symbols:
            return f'sq_ref_for_symbol({self.data_symbols[name]})'
        elif name in self.func_symbols:
            return f'sq_ref_for_symbol({self.func_symbols[name]})'
        else:
            return f'sq_ref_extern("{name}")'

    def _sfloat(self, hex_str):
        import struct
        try:
            val = int(hex_str, 16)
            f = struct.unpack('f', struct.pack('I', val & 0xFFFFFFFF))[0]
            return f'sq_const_single({_fmt_float(f)}f)'
        except (ValueError, struct.error):
            return f'sq_const_single({hex_str}f)'

    def _dfloat(self, hex_str):
        import struct
        try:
            val = int(hex_str, 16)
            d = struct.unpack('d', struct.pack('Q', val & 0xFFFFFFFFFFFFFFFF))[0]
            return f'sq_const_double({_fmt_float(d)})'
        except (ValueError, struct.error):
            return f'sq_const_double({hex_str})'

    # -----------------------------------------------------------------------
    # Top-level
    # -----------------------------------------------------------------------

    def parse(self):
        self.emit_raw('#define SQBE_IMPLEMENTATION')
        self.emit_raw('#include "sqbe.h"')
        self.emit_raw('')
        self.emit_raw('int main(int argc, char** argv) {')
        self.emit('if (argc != 2) {')
        self.indent += 1
        self.emit('fprintf(stderr, "usage: %s <output.s>\\n", argv[0]);')
        self.emit('return 1;')
        self.indent -= 1
        self.emit('}')
        self.emit('SqConfiguration config = SQ_CONFIGURATION_DEFAULT;')
        self.emit('config.output = fopen(argv[1], "wb");')
        self.emit('sq_init(&config);')
        self.emit('')

        while not self.at_end():
            self._parse_toplevel()

        self.emit('')
        self.emit('if (!sq_shutdown()) {')
        self.indent += 1
        self.emit('return 1;')
        self.indent -= 1
        self.emit('}')
        self.emit('fclose(config.output);')
        self.emit('return 0;')
        self.emit_raw('}')

    def _parse_toplevel(self):
        linkage = self._parse_linkage()
        t = self.peek()
        if t.kind == 'WORD' and t.val == 'type':
            self._parse_type_def()
        elif t.kind == 'WORD' and t.val == 'data':
            self._parse_data(linkage)
        elif t.kind == 'WORD' and t.val == 'function':
            self._parse_function(linkage)
        elif t.kind == 'EOF':
            return
        else:
            raise ParseError(f"{self.filename}:{t.line}: unexpected at top level: {t}")

    def _parse_linkage(self):
        exported = False
        while True:
            t = self.peek()
            if t.kind == 'WORD' and t.val == 'export':
                self.advance()
                exported = True
            elif t.kind == 'WORD' and t.val == 'thread':
                if self.peek(1).kind == 'WORD' and self.peek(1).val == 'data':
                    self.advance()  # consume 'thread', 'data' consumed by data parser
                else:
                    break
            elif t.kind == 'WORD' and t.val == 'section':
                self.advance()
                self.expect('STRING')
                if self.peek().kind == 'STRING':
                    self.advance()
            else:
                break
        return 'sq_linkage_export' if exported else 'sq_linkage_default'

    # -----------------------------------------------------------------------
    # Type definitions
    # -----------------------------------------------------------------------

    _FIELD_TYPE_MAP = {
        'b': 'sq_type_byte',  'h': 'sq_type_half',
        'w': 'sq_type_word',  'l': 'sq_type_long',
        's': 'sq_type_single','d': 'sq_type_double',
    }

    def _parse_type_def(self):
        self.expect('WORD', 'type')
        name_tok = self.expect('TYP')
        name = name_tok.val
        self.expect('PUNCT', '=')

        align = 0
        if self.peek().kind == 'WORD' and self.peek().val == 'align':
            self.advance()
            align = int(self.advance().val)

        ty_var = 'ty_' + re.sub(r'[^a-zA-Z0-9_]', '_', name)
        self.emit(f'SqType {ty_var};')
        self.type_map[name] = ty_var

        self.emit('{')
        self.indent += 1
        self.emit(f'sq_type_struct_start("{name}", {align});')

        self.expect('PUNCT', '{')

        t = self.peek()
        if t.kind == 'NUM':
            # Opaque type: { SIZE } — emit empty struct, consume body
            while not (self.peek().kind == 'PUNCT' and self.peek().val == '}'):
                self.advance()
        elif t.kind == 'PUNCT' and t.val == '{':
            # Union: { { variant1 } { variant2 } ... } — use first variant only
            self._parse_union_fields()
        else:
            self._parse_struct_fields()

        self.expect('PUNCT', '}')
        self.emit(f'{ty_var} = sq_type_struct_end();')
        self.indent -= 1
        self.emit('}')
        self.emit('')

    def _parse_struct_fields(self):
        """Parse comma-separated struct field list until next '}'."""
        while not (self.peek().kind == 'PUNCT' and self.peek().val == '}'):
            self._parse_one_type_field()
            if self.peek().kind == 'PUNCT' and self.peek().val == ',':
                self.advance()

    def _parse_union_fields(self):
        """Union: { { f1 } { f2 } } — emit first variant, skip rest."""
        first = True
        while self.peek().kind == 'PUNCT' and self.peek().val == '{':
            self.expect('PUNCT', '{')
            if first:
                self._parse_struct_fields()
                first = False
            else:
                depth = 1
                while not self.at_end() and depth > 0:
                    t = self.advance()
                    if t.kind == 'PUNCT' and t.val == '{': depth += 1
                    elif t.kind == 'PUNCT' and t.val == '}': depth -= 1
                continue
            self.expect('PUNCT', '}')

    def _parse_one_type_field(self):
        """Parse one struct field: (b|h|w|l|s|d|:type) [count]"""
        t = self.peek()
        if t.kind == 'TYP':
            self.advance()
            field_type = self.type_map.get(t.val)
            if field_type is None:
                raise ParseError(f"{self.filename}:{t.line}: unknown type :{t.val}")
        elif t.kind == 'WORD' and t.val in self._FIELD_TYPE_MAP:
            self.advance()
            field_type = self._FIELD_TYPE_MAP[t.val]
        else:
            raise ParseError(f"{self.filename}:{t.line}: expected field type, got {t}")

        if self.peek().kind == 'NUM':
            count = int(self.advance().val)
            if count > 1:
                self.emit(f'sq_type_add_field_with_count({field_type}, {count});')
                return
        self.emit(f'sq_type_add_field({field_type});')

    # -----------------------------------------------------------------------
    # Data definitions
    # -----------------------------------------------------------------------

    def _parse_data(self, linkage):
        self.expect('WORD', 'data')
        name = self.expect('SYM').val
        self.expect('PUNCT', '=')

        # optional: align N
        if self.peek().kind == 'WORD' and self.peek().val == 'align':
            self.advance()
            self.advance()  # consume N (ignore it; sqbe doesn't expose alignment)

        self.expect('PUNCT', '{')

        # Pre-declare the symbol at outer scope so functions can reference it.
        sym_var = mangle_sym(name)
        self.emit(f'SqSymbol {sym_var};')
        self.data_symbols[name] = sym_var

        self.emit('{')
        self.indent += 1
        self.emit(f'sq_data_start({linkage}, "{name}");')

        while not (self.peek().kind == 'PUNCT' and self.peek().val == '}'):
            t = self.peek()
            if t.kind != 'WORD':
                raise ParseError(f"{self.filename}:{t.line}: expected data member type, got {t}")
            mtype = self.advance().val

            if mtype == 'z':
                # zero fill: z N
                n_tok = self.advance()
                n = int(n_tok.val)
                for _ in range(n):
                    self.emit('sq_data_byte(0);')
            elif mtype == 'b':
                t2 = self.peek()
                if t2.kind == 'STRING':
                    self.advance()
                    self.emit(f'sq_data_string("{c_escape(t2.val)}");')
                else:
                    val = self._data_num_val(mtype)
                    self.emit(f'sq_data_byte({val});')
            elif mtype == 'h':
                val = self._data_num_val(mtype)
                self.emit(f'sq_data_half({val});')
            elif mtype == 'w':
                self._emit_data_member('sq_data_word', 'sq_data_ref')
            elif mtype == 'l':
                self._emit_data_member('sq_data_long', 'sq_data_ref')
            elif mtype == 's':
                val = self._data_float_val('s')
                self.emit(f'sq_data_single({val}f);')
            elif mtype == 'd':
                val = self._data_float_val('d')
                self.emit(f'sq_data_double({val});')
            else:
                raise ParseError(f"{self.filename}:{t.line}: unknown data member type: {mtype}")

            if self.peek().kind == 'PUNCT' and self.peek().val == ',':
                self.advance()

        self.expect('PUNCT', '}')
        self.emit(f'{sym_var} = sq_data_end();')
        self.indent -= 1
        self.emit('}')
        self.emit('')

    def _data_num_val(self, mtype):
        """Parse a numeric literal for a data member."""
        t = self.peek()
        if t.kind == 'NUM':
            self.advance()
            return t.val
        raise ParseError(f"{self.filename}:{t.line}: expected number in data member, got {t}")

    def _data_float_val(self, mtype):
        """Parse a float literal for a data member (returns C float string)."""
        t = self.peek()
        if t.kind == 'SFLOAT':
            self.advance()
            return self._sfloat(t.val).replace('sq_const_single(', '').rstrip('f)')
        if t.kind == 'DFLOAT':
            self.advance()
            return self._dfloat(t.val).replace('sq_const_double(', '').rstrip(')')
        if t.kind == 'NUM':
            self.advance()
            return t.val
        raise ParseError(f"{self.filename}:{t.line}: expected float in data member, got {t}")

    def _emit_data_member(self, int_fn, ref_fn):
        """Emit a w or l data member which may be a number or symbol reference."""
        t = self.peek()
        if t.kind == 'SYM':
            sym_name = self.advance().val
            offset = 0
            if self.peek().kind == 'PUNCT' and self.peek().val == '+':
                self.advance()
                offset = int(self.advance().val)
            if sym_name in self.data_symbols:
                sym_var = self.data_symbols[sym_name]
            elif sym_name in self.func_symbols:
                sym_var = self.func_symbols[sym_name]
            else:
                # Forward reference — declare a placeholder (unusual but handle it)
                sym_var = mangle_sym(sym_name)
                self.emit(f'/* warning: forward ref to ${sym_name} */')
            self.emit(f'{ref_fn}({sym_var}, {offset});')
        elif t.kind == 'NUM':
            self.advance()
            self.emit(f'{int_fn}({t.val});')
        else:
            raise ParseError(f"{self.filename}:{t.line}: expected number or symbol in data member, got {t}")

    def _skip_until_closing_brace(self):
        """Skip past the next balanced { ... } including the keyword before it."""
        # Skip tokens until we hit '{'
        while not self.at_end():
            t = self.advance()
            if t.kind == 'PUNCT' and t.val == '{':
                break
        depth = 1
        while not self.at_end() and depth > 0:
            t = self.advance()
            if t.kind == 'PUNCT' and t.val == '{':
                depth += 1
            elif t.kind == 'PUNCT' and t.val == '}':
                depth -= 1

    # -----------------------------------------------------------------------
    # Function parsing
    # -----------------------------------------------------------------------

    def _parse_function(self, linkage):
        self.expect('WORD', 'function')

        ret_type = self.parse_type()
        if ret_type is None:
            ret_type = 'sq_type_void'

        name = self.expect('SYM').val

        # Parse parameters
        self.expect('PUNCT', '(')
        params = []
        while not (self.peek().kind == 'PUNCT' and self.peek().val == ')'):
            if self.peek().kind == 'DOTS':
                self.advance()
                break
            ptype = self.parse_type()
            if ptype is None:
                raise ParseError(f"{self.filename}:{self.peek().line}: expected param type")
            pname = None
            if self.peek().kind == 'TMP':
                pname = self.advance().val
            params.append((ptype, pname))
            if self.peek().kind == 'PUNCT' and self.peek().val == ',':
                self.advance()
        self.expect('PUNCT', ')')
        self.expect('PUNCT', '{')

        # Two-pass: first collect blocks and instructions, then emit.
        # Pass 1: collect block names and raw instructions.
        blocks, instrs = self._collect_blocks()

        # Pass 2: determine forward references.
        param_names = {p[1] for p in params if p[1]}
        forward_refs = self._find_forward_refs(blocks, instrs, param_names)

        # Declare the symbol var at outer scope so later functions can reference it.
        sym_var = mangle_sym(name)
        self.emit(f'SqSymbol {sym_var};')
        self.func_symbols[name] = sym_var

        # Each function body in its own C scope to avoid name collisions.
        self.emit('{')
        self.indent += 1
        # Emit function start.
        self.emit(f'sq_func_start({linkage}, {ret_type}, "{name}");')
        for ptype, pname in params:
            if pname:
                self.emit(f'SqRef {mangle_tmp(pname)} = sq_func_param_named({ptype}, "{pname}");')
            else:
                self.emit(f'sq_func_param({ptype});')

        # Emit forward declarations.
        for tmp_name in sorted(forward_refs):
            self.emit(f'SqRef {mangle_tmp(tmp_name)} = sq_ref_declare();')

        # Track declared SqRef names for this function to avoid duplicate decls.
        self.declared_vars = set(forward_refs) | set(param_names)

        # Pre-declare all blocks.
        if blocks:
            self.emit(f'SqBlock {mangle_block(blocks[0])} = sq_func_get_entry_block();')
            for blk in blocks[1:]:
                self.emit(f'SqBlock {mangle_block(blk)} = sq_block_declare_named("{blk}");')

        # Emit blocks and instructions.
        for i, blk in enumerate(blocks):
            self.emit('')
            if i > 0:
                self.emit(f'sq_block_start({mangle_block(blk)});')
            for raw_instr in instrs[blk]:
                self._emit_instr(raw_instr, forward_refs)

        self.emit(f'{sym_var} = sq_func_end();')
        self.indent -= 1
        self.emit('}')
        self.emit('')

    def _collect_blocks(self):
        """Collect block names and tokenized instructions from function body."""
        blocks = []
        instrs = {}  # block_name -> list of (dest, type, op, token_ranges)
        current_block = None

        while not (self.peek().kind == 'PUNCT' and self.peek().val == '}'):
            t = self.peek()
            if t.kind == 'BLK':
                self.advance()
                current_block = t.val
                blocks.append(current_block)
                instrs[current_block] = []
            elif current_block is not None:
                raw = self._parse_one_instr()
                instrs[current_block].append(raw)
            else:
                raise ParseError(f"{self.filename}:{t.line}: instruction outside block")

        self.expect('PUNCT', '}')
        return blocks, instrs

    def _parse_one_instr(self):
        """Parse one instruction, returning a structured dict.

        Each instruction knows its own argument grammar, so we parse
        exactly the right number of tokens.
        """
        t = self.peek()

        # Check for assignment: %tmp =T op args
        dest = None
        itype = None
        if t.kind == 'TMP' and self.peek(1).kind == 'PUNCT' and self.peek(1).val == '=':
            dest = self.advance().val
            self.expect('PUNCT', '=')
            itype = self.parse_type()

        op = self.expect('WORD').val
        args = self._parse_instr_args(op)
        return {'dest': dest, 'type': itype, 'op': op, 'args': args}

    def _parse_raw_val(self):
        """Parse a value, returning raw (kind, val) tuple for later resolution."""
        t = self.peek()
        if t.kind in ('TMP', 'SYM', 'NUM', 'SFLOAT', 'DFLOAT'):
            self.advance()
            return (t.kind, t.val)
        else:
            raise ParseError(f"{self.filename}:{t.line}: expected value, got {t}")

    def _resolve_raw_val(self, raw):
        """Resolve a raw (kind, val) tuple into C code string."""
        kind, val = raw
        if kind == 'TMP':
            return mangle_tmp(val)
        elif kind == 'SYM':
            return self._resolve_sym(val)
        elif kind == 'NUM':
            return f'sq_const_int({val})'
        elif kind == 'SFLOAT':
            return self._sfloat(val)
        elif kind == 'DFLOAT':
            return self._dfloat(val)
        elif kind == 'BLK':
            return mangle_block(val)
        else:
            raise ParseError(f"cannot resolve value: ({kind}, {val})")

    def _parse_instr_args(self, op):
        """Parse instruction arguments based on the specific instruction."""
        if op == 'ret':
            t = self.peek()
            if t.kind in ('TMP', 'SYM', 'NUM', 'SFLOAT', 'DFLOAT'):
                return [self._parse_raw_val()]
            return []

        if op in _TWO_OP_TYPED:
            a0 = self._parse_raw_val()
            self.expect('PUNCT', ',')
            a1 = self._parse_raw_val()
            return [a0, a1]

        if op in _ONE_OP_TYPED:
            return [self._parse_raw_val()]

        if op in _VOID_ONE_OP:
            return [self._parse_raw_val()]

        if op in _STORE_INSTRS:
            # storew val, addr  (no type, no dest)
            a0 = self._parse_raw_val()
            self.expect('PUNCT', ',')
            a1 = self._parse_raw_val()
            return [a0, a1]

        if op in _LOAD_GENERIC or op in _LOAD_EXT:
            return [self._parse_raw_val()]

        if op in _ALLOC_INSTRS:
            return [self._parse_raw_val()]

        if op == 'blit':
            # blit src, dst, N
            src = self._parse_raw_val()
            self.expect('PUNCT', ',')
            dst = self._parse_raw_val()
            self.expect('PUNCT', ',')
            n = self._parse_raw_val()
            return [src, dst, n]

        if op == 'jmp':
            # jmp @label
            blk = self.expect('BLK')
            return [('BLK', blk.val)]

        if op == 'jnz':
            # jnz %cond, @true, @false
            cond = self._parse_raw_val()
            self.expect('PUNCT', ',')
            btrue = self.expect('BLK')
            self.expect('PUNCT', ',')
            bfalse = self.expect('BLK')
            return [cond, ('BLK', btrue.val), ('BLK', bfalse.val)]

        if op == 'phi':
            # phi @b0 val0, @b1 val1
            pairs = []
            while len(pairs) < 4 and self.peek().kind == 'BLK':
                blk = self.expect('BLK')
                val = self._parse_raw_val()
                pairs.append(('BLK', blk.val))
                pairs.append(val)
                if self.peek().kind == 'PUNCT' and self.peek().val == ',':
                    self.advance()
            if len(pairs) != 4:
                raise ParseError(
                    f"{self.filename}:{self.peek().line}: "
                    f"phi requires exactly 2 predecessors, got {len(pairs)//2}"
                )
            return pairs

        if op == 'call':
            # call callee(T0 arg0, T1 arg1, ..., T argN)
            # callee is $sym or %tmp
            t = self.peek()
            if t.kind == 'SYM':
                callee = ('SYM', self.advance().val)
            elif t.kind == 'TMP':
                callee = ('TMP', self.advance().val)
            else:
                raise ParseError(f"{self.filename}:{t.line}: expected callee, got {t}")
            self.expect('PUNCT', '(')
            call_args = []
            while not (self.peek().kind == 'PUNCT' and self.peek().val == ')'):
                if self.peek().kind == 'DOTS':
                    self.advance()
                    call_args.append(('VARARGS',))
                    if self.peek().kind == 'PUNCT' and self.peek().val == ',':
                        self.advance()
                    continue
                atype = self.parse_type()
                if atype is None:
                    raise ParseError(
                        f"{self.filename}:{self.peek().line}: expected arg type in call"
                    )
                aval = self._parse_raw_val()
                call_args.append(('ARG', atype, aval))
                if self.peek().kind == 'PUNCT' and self.peek().val == ',':
                    self.advance()
            self.expect('PUNCT', ')')
            return [callee] + call_args

        if op == 'hlt':
            return []

        raise ParseError(f"unsupported instruction: {op}")

    def _find_forward_refs(self, blocks, instrs, param_names):
        """Find temporaries used before defined (excluding params)."""
        defined = set(param_names)
        used_before_def = set()
        for blk in blocks:
            for raw in instrs[blk]:
                for a in raw['args']:
                    if isinstance(a, tuple) and a[0] == 'TMP' and a[1] not in defined:
                        used_before_def.add(a[1])
                if raw['dest']:
                    defined.add(raw['dest'])
        return used_before_def

    def _emit_instr(self, raw, forward_refs):
        """Emit C code for one instruction."""
        op = raw['op']
        dest = raw['dest']
        itype = raw['type']
        args = raw['args']

        if op == 'ret':
            if args:
                self.emit(f'sq_i_ret({self._resolve_raw_val(args[0])});')
            else:
                self.emit('sq_i_ret_void();')
            return

        if op in _TWO_OP_TYPED:
            a0 = self._resolve_raw_val(args[0])
            a1 = self._resolve_raw_val(args[1])
            ctype = itype or 'sq_type_word'
            self._emit_dest(dest, forward_refs, f'sq_i_{op}', f'{ctype}, {a0}, {a1}')
            return

        if op in _ONE_OP_TYPED:
            a0 = self._resolve_raw_val(args[0])
            ctype = itype or 'sq_type_word'
            self._emit_dest(dest, forward_refs, f'sq_i_{op}', f'{ctype}, {a0}')
            return

        if op in _VOID_ONE_OP:
            a0 = self._resolve_raw_val(args[0])
            self.emit(f'sq_i_{op}({a0});')
            return

        if op in _STORE_INSTRS:
            a0 = self._resolve_raw_val(args[0])
            a1 = self._resolve_raw_val(args[1])
            self.emit(f'sq_i_{op}({a0}, {a1});')
            return

        if op in _LOAD_GENERIC:
            fixed_type = _LOAD_GENERIC[op]
            a0 = self._resolve_raw_val(args[0])
            self._emit_dest(dest, forward_refs, 'sq_i_load', f'{fixed_type}, {a0}')
            return

        if op in _LOAD_EXT:
            a0 = self._resolve_raw_val(args[0])
            ctype = itype or 'sq_type_word'
            self._emit_dest(dest, forward_refs, f'sq_i_{op}', f'{ctype}, {a0}')
            return

        if op in _ALLOC_INSTRS:
            a0 = self._resolve_raw_val(args[0])
            self._emit_dest(dest, forward_refs, f'sq_i_{op}', a0)
            return

        if op == 'blit':
            src = self._resolve_raw_val(args[0])
            dst = self._resolve_raw_val(args[1])
            n   = self._resolve_raw_val(args[2])
            self.emit(f'sq_i_blit({src}, {dst}, {n});')
            return

        if op == 'jmp':
            self.emit(f'sq_i_jmp({mangle_block(args[0][1])});')
            return

        if op == 'jnz':
            cond = self._resolve_raw_val(args[0])
            btrue  = mangle_block(args[1][1])
            bfalse = mangle_block(args[2][1])
            self.emit(f'sq_i_jnz({cond}, {btrue}, {bfalse});')
            return

        if op == 'hlt':
            self.emit('sq_i_hlt();')
            return

        if op == 'call':
            # args = [callee, ...call_args]
            callee_raw = args[0]
            if callee_raw[0] == 'SYM':
                func_expr = self._resolve_sym(callee_raw[1])
            else:
                func_expr = mangle_tmp(callee_raw[1])
            result_type = itype or 'sq_type_void'
            # Build SqCallArg list
            ca_parts = []
            for a in args[1:]:
                if a[0] == 'VARARGS':
                    ca_parts.append('sq_varargs_begin')
                else:
                    _, atype, aval = a
                    aval_str = self._resolve_raw_val(aval)
                    ca_parts.append(f'(SqCallArg){{{atype}, {aval_str}}}')
            n = len(ca_parts)
            if n <= 8:
                if n == 0:
                    call_expr = f'sq_i_call0({result_type}, {func_expr})'
                else:
                    call_expr = f'sq_i_call{n}({result_type}, {func_expr}, {", ".join(ca_parts)})'
            else:
                # sq_i_calla: build a compound array literal
                arr = '{' + ', '.join(ca_parts) + '}'
                call_expr = f'sq_i_calla({result_type}, {func_expr}, {n}, (SqCallArg[]){arr})'
            if dest is None:
                self.emit(f'{call_expr};')
            elif dest in self.declared_vars:
                self.emit(f'{mangle_tmp(dest)} = {call_expr};')
            else:
                self.declared_vars.add(dest)
                self.emit(f'SqRef {mangle_tmp(dest)} = {call_expr};')
            return

        if op == 'phi':
            # args = [('BLK', b0), val0, ('BLK', b1), val1]
            b0  = mangle_block(args[0][1])
            v0  = self._resolve_raw_val(args[1])
            b1  = mangle_block(args[2][1])
            v1  = self._resolve_raw_val(args[3])
            ctype = itype or 'sq_type_word'
            # phi has no _into variant; dest is always freshly defined here
            if dest in forward_refs or dest in self.declared_vars:
                # already declared — just assign (no SqRef re-declaration)
                self.emit(f'{mangle_tmp(dest)} = sq_i_phi({ctype}, {b0}, {v0}, {b1}, {v1});')
            else:
                self.declared_vars.add(dest)
                self.emit(f'SqRef {mangle_tmp(dest)} = sq_i_phi({ctype}, {b0}, {v0}, {b1}, {v1});')
            return

        raise ParseError(f"unsupported instruction in emit: {op}")

    def _emit_dest(self, dest, forward_refs, func, args_str):
        """Emit an instruction call, handling dest assignment and _into variants."""
        if dest is None:
            self.emit(f'{func}({args_str});')
        elif dest in forward_refs:
            self.emit(f'{func}_into({mangle_tmp(dest)}, {args_str});')
        elif dest in self.declared_vars:
            # Re-definition of same name in a different block — assign without redeclaring.
            self.emit(f'{mangle_tmp(dest)} = {func}({args_str});')
        else:
            self.declared_vars.add(dest)
            self.emit(f'SqRef {mangle_tmp(dest)} = {func}({args_str});')


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _fmt_float(f):
    import math
    if math.isinf(f):
        return '(1.0/0.0)' if f > 0 else '(-1.0/0.0)'
    if math.isnan(f):
        return '(0.0/0.0)'
    s = repr(f)
    if '.' not in s and 'e' not in s and 'E' not in s:
        s += '.0'
    return s


def c_escape(s):
    r = ''
    for c in s:
        if c == '\\': r += '\\\\'
        elif c == '"': r += '\\"'
        elif c == '\n': r += '\\n'
        elif c == '\t': r += '\\t'
        elif c == '\0': r += '\\0'
        elif ord(c) < 32 or ord(c) > 126: r += f'\\x{ord(c):02x}'
        else: r += c
    return r


def has_multiway_phi(text):
    """Check if .ssa has phi nodes with 3+ predecessors."""
    for line in text.split('\n'):
        line = line.strip()
        if '=' not in line:
            continue
        parts = line.split('=', 1)
        if len(parts) != 2:
            continue
        rhs = parts[1].strip()
        # Strip optional type prefix (w, l, s, d, sb, ub, sh, uh)
        for t in ('sb ', 'ub ', 'sh ', 'uh ', 'w ', 'l ', 's ', 'd '):
            if rhs.startswith(t):
                rhs = rhs[len(t):]
                break
        if rhs.startswith('phi '):
            if rhs.count('@') > 2:
                return True
    return False


def translate(text, filename="<stdin>"):
    tokens = tokenize(text)
    parser = Parser(tokens, filename)
    parser.parse()
    return '\n'.join(parser.lines) + '\n'


def main():
    if len(sys.argv) < 2:
        print(f"usage: {sys.argv[0]} <file.ssa>", file=sys.stderr)
        sys.exit(1)

    filename = sys.argv[1]
    with open(filename) as f:
        text = f.read()

    if has_multiway_phi(text):
        print(f"error: {filename} has 3+ way phi nodes (not supported)", file=sys.stderr)
        sys.exit(2)

    try:
        c_code = translate(text, filename)
        print(c_code, end='')
    except ParseError as e:
        print(f"error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
