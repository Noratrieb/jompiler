import { spawn } from "node:child_process";
import fs from "node:fs/promises";

// https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
// yep.

const BUFFER_LE = true;

class CompilerError extends Error {
  constructor(message, span) {
    super(message);
    assertDefined(message);
    assertDefined(span);
    this.span = span;
  }

  render(sourceFileName, source) {
    const lines = source.split("\n");
    let total = 0;
    let lineNumber = -1;
    let colNumber = -1;
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const newTotal = total + line.length + 1;
      if (newTotal > this.span) {
        lineNumber = i + 1;
        colNumber = this.span - total + 1;
        break;
      }
      total = newTotal;
    }

    return `error: ${sourceFileName}:${lineNumber}:${colNumber}: ${this.message}`;
  }
}

function lex(input) {
  // 6.4.2 Identifiers
  function identifierStart(char) {
    return (
      (char >= "a" && char <= "z") ||
      (char >= "A" && char <= "Z") ||
      char === "_"
    );
  }
  function identifierCont(char) {
    return identifierStart(char) || (char >= "0" && char <= "9");
  }

  const tokens = [];
  let i = 0;
  while (i < input.length) {
    const head = input[i];
    i++;
    if (head === " " || head === "\t" || head === "\n") {
      continue;
    }
    if (head === "/" && input[i] === "/") {
      while (input[i] !== "\n") {
        i++;
      }
      continue;
    }
    if (head >= "0" && head <= "9") {
      const span = i - 1;
      let number = head;
      while (input[i] >= "0" && input[i] <= "9") {
        number += input[i];
        i++;
      }
      tokens.push({
        kind: "integer",
        integer: Number(number),
        span,
      });
    } else if (identifierStart(head)) {
      const span = i - 1;
      let ident = head;
      while (identifierCont(input[i])) {
        ident += input[i];
        i++;
      }
      tokens.push({
        kind: "ident",
        ident,
        span,
      });
    } else if (head === '"') {
      const span = i - 1;
      let string = "";
      while (input[i] !== '"') {
        string += input[i];
        i++;
      }
      i++; // "
      tokens.push({
        kind: "string",
        string,
        span,
      });
    } else {
      switch (head) {
        case "(":
        case ")":
        case "{":
        case "}":
        case ",":
        case ";":
        case "+":
        case "-":
        case "=": {
          tokens.push({
            kind: head,
            span: i - 1,
          });
          break;
        }
        default: {
          throw new CompilerError(`unexpected character '${head}'`, i - 1);
        }
      }
    }
  }
  return tokens;
}

function parse(tokens) {
  const tokenIter = {
    tokens,
    nextIndex: 0,
    hasNext() {
      return this.nextIndex < tokens.length;
    },
    peek() {
      if (!this.hasNext()) {
        return undefined;
      }
      return this.tokens[this.nextIndex];
    },
    next(description) {
      if (!this.hasNext()) {
        throw new CompilerError(
          `unexpected end of file while parsing ${description}`
        );
      }

      return this.tokens[this.nextIndex++];
    },
    expect(kind, description) {
      const tok = this.next(description);
      if (tok.kind !== kind) {
        throw new CompilerError(
          `expected '${kind}' for ${description}, found ${tok.kind}`,
          tok.span
        );
      }
      return tok;
    },
  };

  function parseType(tok, description) {
    const first = tok.next(`type for ${description}`);
    if (first.kind === "ident") {
      switch (first.ident) {
        case "int": {
          return {
            kind: "int",
            span: first.span,
          };
        }
      }
    } else {
      throw new CompilerError(
        `unexpected token ${first.kind} while parsing type for ${description}`,
        first.span
      );
    }
  }

  function parseExpr(tok) {
    function generateBinaryParser(ops, higher) {
      return function parseBinaryExpr(tok) {
        const lhs = higher(tok);
        if (ops.includes(tok.peek()?.kind)) {
          const op = tok.next("binary operator");
          const rhs = parseBinaryExpr(tok);

          return {
            kind: op.kind,
            span: op.span,
            lhs,
            rhs,
          };
        }

        return lhs;
      };
    }

    function parsePrimary(tok) {
      const head = tok.next("expression");
      switch (head.kind) {
        case "string": {
          return {
            kind: "string",
            string: head.string,
            span: head.span,
          };
        }
        case "ident": {
          return {
            kind: "ident",
            string: head.ident,
            span: head.span,
          };
        }
        case "integer": {
          return {
            kind: "integer",
            integer: head.integer,
            span: head.span,
          };
        }
        default: {
          throw new CompilerError(
            `unexpected token ${head.kind} in expression`,
            head.span
          );
        }
      }
    }

    function parsePostfix(tok) {
      let lhs = parsePrimary(tok);

      while (true) {
        const next = tok.peek()?.kind;
        if (next === "(") {
          const popen = tok.next("open parenthesis of call");

          const args = [];
          let isFirst = true;
          while (tok.peek()?.kind !== ")") {
            if (!isFirst) {
              tok.expect(",", "comma between arguments");
            }
            const next = parseExpr(tok);
            args.push(next);
          }
          tok.expect(")", "closing parenthesis of call");

          lhs = {
            kind: "call",
            lhs,
            args,
            span: popen.span,
          };
        } else {
          break;
        }
      }

      return lhs;
    }

    const parseMultiplicative = generateBinaryParser(
      ["*", "/", "%"],
      parsePostfix
    );
    const parseAdditive = generateBinaryParser(["+", "-"], parseMultiplicative);
    const parseShift = generateBinaryParser(["<<", ">>"], parseAdditive);
    const parseRelational = generateBinaryParser(
      ["<", ">", "<=", ">="],
      parseShift
    );
    const parseEquality = generateBinaryParser(["==", "!="], parseRelational);
    const parseAnd = generateBinaryParser(["&"], parseEquality);
    const parseExclusiveOr = generateBinaryParser(["^"], parseAnd);
    const parseInclusiveOr = generateBinaryParser(["|"], parseExclusiveOr);
    const parseLogicalAnd = generateBinaryParser(["&&"], parseInclusiveOr);
    const parseLogicalOr = generateBinaryParser(["||"], parseLogicalAnd);
    // TODO conditional operator
    const parseAssignment = generateBinaryParser(
      ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", , "^=", "|="],
      parseLogicalOr
    );
    const parseComma = generateBinaryParser([","], parseAssignment);

    return parseComma(tok);
  }

  function parseStatement(tok) {
    switch (tok.peek()?.kind) {
      case "ident": {
        switch (tok.peek()?.ident) {
          case "return": {
            const span = tok.next("return").span;
            let rhs = undefined;
            if (tok.peek()?.kind !== ";") {
              rhs = parseExpr(tok);
            }

            return {
              kind: "return",
              rhs,
              span,
            };
          }
          case "int": {
            const typeSpan = tok.next("ident").span;
            const name = tok.next("ident");

            tok.next("=");

            let init;
            if (tok.peek()?.kind !== ";") {
              init = parseExpr(tok);
            }

            return {
              kind: "declaration",
              type: "int",
              name: name.ident,
              span: typeSpan,
              init,
            };
          }
          default: {
            // fallthrough
          }
        }
      }
      default: {
        const expr = parseExpr(tok);
        return {
          kind: "expr",
          expr,
          span: expr.span,
        };
      }
    }
  }

  function parseBlock(tok) {
    tok.expect("{", "start of block");

    const statements = [];

    while (tok.peek()?.kind !== "}") {
      statements.push(parseStatement(tok));
      tok.expect(";", "end of statement");
    }

    tok.expect("}", "end of block");

    return statements;
  }

  function parseDeclaration(tok) {
    const ret = parseType(tok, "function return");
    const name = tok.expect("ident", "function name");

    tok.expect("(", "function parentheses");

    const params = [];

    while (tok.peek()?.kind !== ")") {
      if (params.length > 0) {
        tok.expect(",", "function parameter separator");
      }
      const type = parseType(tok, "function parameter");
      const name = tok.next("function name");
      if (name.kind !== "ident") {
        throw new CompilerError(
          `expected identifier for function parameter, found ${name.kind}`,
          name.span
        );
      }
      params.push({
        type,
        name: name.ident,
      });
    }

    tok.expect(")", "function parentheses");

    const body = parseBlock(tok);

    return {
      kind: "function",
      name,
      params,
      ret,
      body,
    };
  }

  const decls = [];
  while (tokenIter.hasNext()) {
    const decl = parseDeclaration(tokenIter);
    decls.push(decl);
  }

  return decls;
}

function lower(ast) {
  /*
  x86-64 codegen.

  strategy: every expression returns its result in rax.

  useful:
  - https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
  - https://gitlab.com/x86-psABIs/x86-64-ABI
  */

  function littleEndian16(number) {
    assertDefined(number);
    assert(typeof number === "number");
    return [number & 0xff, (number >> 8) & 0xff];
  }
  function littleEndian32(number) {
    assertDefined(number);
    assert(typeof number === "number");
    return [
      number & 0xff,
      (number >> 8) & 0xff,
      (number >> 16) & 0xff,
      (number >> 24) & 0xff,
    ];
  }
  function littleEndian64(number) {
    assertDefined(number);
    assert(number <= 0xff_ff_ff_ff);
    assert(typeof number === "number");
    return [...littleEndian32(number), 0, 0, 0, 0];
  }
  function signedLittleEndian64(number) {
    assertDefined(number);
    assert(number <= 0xff_ff_ff_ff);
    assert(number >= -(0xff_ff_ff_ff + 1));
    assert(typeof number === "number");

    const array = littleEndian64(number);
    const signBit = array[3] & 0b10000000;
    if (signBit) {
      array[4] = 0xff;
      array[5] = 0xff;
      array[6] = 0xff;
      array[7] = 0xff;
    }
    return array;
  }

  const RELOCATIONS = {
    R_X86_64_PC32: 2,
    R_X86_64_PLT32: 4,
  };
  const SYMBOL_TYPES = {
    STT_NOTYPE: 0,
    STT_FUNC: 2,
  };
  const SYMBOL_BINDING = {
    STB_LOCAL: 0,
    STB_GLOBAL: 1,
  };
  const SYMBOL_VISIBILITY = {
    STV_DEFAULT: 0,
  };

  // 2.1.3 ModR/M and SIB Bytes
  const MOD_REG = 0b11;

  const RM_A = 0b000;
  const RM_C = 0b001;
  const RM_D = 0b010;
  const RM_B = 0b011;
  const RM_SP = 0b100;
  const RM_BP = 0b101;
  const RM_SI = 0b110;
  const RM_DI = 0b111;

  const REG_A = RM_A;
  const REG_C = RM_C;
  const REG_D = RM_D;
  const REG_B = RM_B;
  const REG_SP = RM_SP;
  const REG_BP = RM_BP;
  const REG_SI = RM_SI;
  const REG_DI = RM_DI;

  const PARAM_CALLCONV_REGISTERS = [REG_DI, REG_SI, REG_D, REG_C];

  const REG_IGNORED = 0;
  function modRm(mod, reg, rm) {
    assert(mod <= 0b11);
    assert(rm <= 0b111);
    assert(reg <= 0b111);
    return (mod << 6) | (reg << 3) | rm;
  }

  const REX = {
    W_OPERAND_SIZE_DETERMINED: 0,
    W_64_BIT_OPERAND_SIZE: 1,
  };

  function rex(w, r, x, b) {
    assert(w <= 1);
    assert(r <= 1);
    assert(x <= 1);
    assert(b <= 1);
    return 0b0100_0000 | (w << 3) | (r << 2) | (x << 1) | b;
  }

  class InstBuilder {
    /**
     * The reserved stack space for locals and intermediary values.
     * We subtract rsp by this value in the beginning.
     */
    #stackSize;
    #patches;
    constructor() {
      this.out = new Uint8Array();
      this.relocations = [];
      this.#stackSize = 0;
      this.#patches = [];

      this.#prologue();
    }

    #prologue() {
      // push rbp
      this.pushReg64(REG_BP); // push british petroleum
      // mov rbp, rsp
      this.movRegReg64(REG_BP, REG_SP);
      // sub rsp, SIZE
      this.subImm(REG_SP, 0);
      this.#patches.push({
        start: this.out.length - 4,
        patch: () => littleEndian32(this.#stackSize),
      });
    }

    #epilogue() {
      // mov rsp, rbp
      this.movRegReg64(REG_SP, REG_BP);
      // pop rbp
      this.popReg64(REG_BP);
    }

    finish() {
      this.#epilogue();
      this.#patches.forEach((patch) => {
        const result = patch.patch();
        assert(Array.isArray(result));
        result.forEach((v, i) => {
          this.out[patch.start + i] = v;
        });
      });
    }

    reserveStack(size) {
      const prev = this.#stackSize;
      this.#stackSize += size;
      return prev;
    }

    addRegs(reg1, reg2) {
      // REX.W + 03 /r | ADD r64, r/m64 ; Add r/m64 to r64
      this.#append([
        rex(REX.W_64_BIT_OPERAND_SIZE, 0, 0, 0),
        0x03,
        modRm(MOD_REG, reg1, reg2),
      ]);
    }

    subRegs(reg1, reg2) {
      // REX.W + 03 /r | ADD r64, r/m64 ; Add r/m64 to r64
      this.#append([
        rex(REX.W_64_BIT_OPERAND_SIZE, 0, 0, 0),
        0x2b,
        modRm(MOD_REG, reg1, reg2),
      ]);
    }

    movRegToStackOffset(offset, reg) {
      // mov [rsp+{offset}], reg
      // 89 /r, MOV r/m64, r64
      this.#append([
        0x89,
        // [--][--]+disp32
        modRm(0b10, reg, 0b100),
        0x24, // SIB: Scaled index: none, Base: ESP
        ...littleEndian32(offset),
      ]);
    }

    movStackOffsetToReg(offset, reg) {
      // mov reg, [rsp+{offset}]
      // 89 /r, MOV r/m64, r64
      this.#append([
        0x8b,
        // [--][--]+disp32
        modRm(0b10, reg, 0b100),
        0x24, // SIB: Scaled index: none, Base: ESP
        ...littleEndian32(offset),
      ]);
    }

    movEaxImm32(imm) {
      // mov eax, imm
      this.#append([
        0xc7,
        modRm(MOD_REG, REG_IGNORED, RM_A),
        ...littleEndian32(imm),
      ]);
    }

    movRegReg32(to, from) {
      // ; Move r/m32 to r32
      this.#append([0x8b, modRm(MOD_REG, to, from)]);
    }

    movRegReg64(to, from) {
      // ; Move r/m64 to r64.
      this.#append([
        rex(REX.W_64_BIT_OPERAND_SIZE, 0, 0, 0),
        0x8b,
        modRm(MOD_REG, to, from),
      ]);
    }

    pushReg64(reg) {
      // 58+rd ; Push r64.
      this.#append([0x50 | reg]);
    }

    popReg64(reg) {
      // 50+rd ; Pop top of stack into r64; increment stack pointer.
      this.#append([0x58 | reg]);
    }

    subImm(reg, imm) {
      // REX.W + 81 /5 id  ; Subtract imm32 sign-extended to 64-bits from r/m64.
      this.#append([
        rex(REX.W_64_BIT_OPERAND_SIZE, 0, 0, 0),
        0x81,
        modRm(MOD_REG, 5 /* /5*/, reg),
        ...littleEndian32(imm),
      ]);
    }

    call(symbol) {
      // call rel32 ; Call near, relative, displacement relative to next
      //            ; instruction. 32-bit displacement sign extended to
      //            ; 64-bits in 64-bit mode
      this.#append([0xe8]);
      this.relocations.push({
        kind: RELOCATIONS.R_X86_64_PC32, // pietro said i should use this even though PC_32 works too
        symbol,
        offset: this.out.length,
        addend: -4,
      });
      this.#append([0x0, 0x0, 0x0, 0x0]);
    }

    ret() {
      // ret ; near return to calling prodecude
      this.#append([0xc3]);
    }

    #append(code) {
      this.out = Buffer.concat([this.out, new Uint8Array(code)]);
    }
  }

  function codegenExpr(ctx, expr) {
    assert(!Number.isNaN(ctx.offset));
    const { ib, variables } = ctx;
    switch (expr.kind) {
      case "call": {
        if (expr.lhs.kind !== "ident") {
          throw new Error("bad");
        }
        if (expr.args.length !== 1) {
          throw new Error("bad");
        }

        // TODO: save
        codegenExpr(ctx, expr.args[0]);
        // mov edi, eax
        ib.movRegReg32(REG_DI, REG_A);
        ib.call(expr.lhs.string);

        break;
      }
      case "integer": {
        ib.movEaxImm32(expr.integer);
        break;
      }
      case "ident": {
        const offset = [...variables]
          .reverse()
          .find((v) => v.name === expr.string);
        assert(offset);
        ib.movStackOffsetToReg(offset.stackOffset + ctx.offset, REG_A);
        break;
      }
      case "+": {
        // For binary expressions, we first evaluate the LHS, save it on the stack,
        // evaluate the RHS, then restore the LHS and perform the operation.
        codegenExpr(ctx, expr.lhs);
        ib.pushReg64(REG_A); // push rax
        codegenExpr({ ...ctx, offset: ctx.offset + 8 }, expr.rhs);
        ib.popReg64(REG_C); // pop rcx

        ib.addRegs(REG_A, REG_C);
        break;
      }
      case "-": {
        codegenExpr(ctx, expr.rhs);
        ib.pushReg64(REG_A); // push rax
        codegenExpr({ ...ctx, offset: ctx.offset + 8 }, expr.lhs);
        ib.popReg64(REG_C); // pop rcx

        ib.subRegs(REG_A, REG_C);
        break;
      }
      default: {
        throw new Error(`unsupported expr: ${expr.kind}`);
      }
    }
  }

  function codegenFunction(func) {
    const ib = new InstBuilder();
    const variables = [];

    assert(func.params.length <= 4);

    func.params.forEach((param, i) => {
      assert(param.type.kind === "int");
      const offset = ib.reserveStack(4);
      ib.movRegToStackOffset(offset, PARAM_CALLCONV_REGISTERS[i]);
      variables.push({
        name: param.name,
        stackOffset: offset,
      });
    });

    for (const stmt of func.body) {
      const ctx = { ib, variables, offset: 0 };
      switch (stmt.kind) {
        case "expr": {
          codegenExpr(ctx, stmt.expr);
          break;
        }
        case "return": {
          if (stmt.rhs) {
            codegenExpr(ctx, stmt.rhs);
          }
          ib.finish();
          ib.ret();
          break;
        }
        case "declaration": {
          assert(stmt.type === "int");
          if (stmt.init) {
            codegenExpr(ctx, stmt.init);
          }
          const slot = ib.reserveStack(4);
          variables.push({
            name: stmt.name,
            stackOffset: slot,
          });
          // mov [rsp+{slot}], eax
          ib.movRegToStackOffset(slot, REG_A);
          break;
        }
        default: {
          assert(false);
        }
      }
    }

    ib.movEaxImm32(0);
    ib.finish();
    ib.ret();

    return ib;
  }

  class BufferBuilder {
    #buffer
    constructor() {
      this.#buffer = new ArrayBuffer(0, {maxByteLength: 2**32});
    }
    append(array) {
      assertDefined(array);
      array.forEach((elem) => assert(typeof elem === "number"));

      const oldLength = this.length;
      const newLength = oldLength + array.length;
      this.#buffer.resize(newLength);

      for (let i = 0; i < array.length; i++) {
        new DataView(this.#buffer).setUint8(oldLength + i, array[i]);
      }
    }
    get length() {
      return this.#buffer.byteLength;
    }
    get currentPos() {
      return this.length;
    }
    writeUint16Le(offset, int) {
      new DataView(this.#buffer).setUint16(offset, int, BUFFER_LE);
    }
    writeUint32Le(offset, int) {
      new DataView(this.#buffer).setUint32(offset, int, BUFFER_LE);
    }
    toUint8Array() {
      return new Uint8Array(this.#buffer);
    }
  }

  function generateObjectFile(funcs) {
    const alignTo = (out, align) => {
      assertDefined(out, align);
      const missing = out.length % align;
      if (missing === 0) {
        return;
      }
      const up = align - missing;
      out.append(Array(up).fill(0));
    };

    function layoutFuncs(funcs) {
      const textContent = new BufferBuilder();

      const textRelativeSymbols = [];
      const relocations = [];

      funcs.forEach((func) => {
        alignTo(textContent, 8); // i think this is not actually necessary.
        const offset = textContent.length;
        textRelativeSymbols.push({
          name: func.name,
          offset,
          size: func.code.length,
        });
        relocations.push(
          ...func.relocations.map((relocation) => ({
            kind: relocation.kind,
            symbol: relocation.symbol,
            addend: relocation.addend,
            offset: offset + relocation.offset,
          }))
        );
        textContent.append(func.code);
      });

      return {
        textContent: textContent.toUint8Array(),
        textRelativeSymbols,
        relocations,
      };
    }

    const symbols = [
      {
        name: "",
        type: SYMBOL_TYPES.STT_NOTYPE,
        binding: SYMBOL_BINDING.STB_LOCAL,
        visibility: SYMBOL_VISIBILITY.STV_DEFAULT,
        sectionIndex: 0,
        value: 0,
        size: 0,
      },
    ];

    const {
      textContent,
      textRelativeSymbols,
      relocations: funcRelocations,
    } = layoutFuncs(funcs);

    for (const sym of textRelativeSymbols) {
      symbols.push({
        name: sym.name,
        type: SYMBOL_TYPES.STT_FUNC,
        binding: SYMBOL_BINDING.STB_GLOBAL,
        visibility: SYMBOL_VISIBILITY.STV_DEFAULT,
        sectionIndex: 1 /*.text*/,
        value: sym.offset,
        size: sym.size,
      });
    }

    console.log("relocations", funcRelocations);

    let out = new BufferBuilder();
    // ident
    out.append([0x7f, "E".charCodeAt(0), "L".charCodeAt(0), "F".charCodeAt(0)]);
    out.append([
      /*ELFCLASS64*/ 2, /*ELFDATA2LSB*/ 1, /*EV_CURRENT*/ 1,
      /*ELFOSABI_SYSV*/ 0, /*EI_ABIVERSION*/ 0, /*EI_PAD*/ 0, 0, 0, 0, 0, 0, 0,
    ]);

    let shoffRef;
    let shnumRef;
    let shstrndxRef;
    let sectionOffsetRefs = {};

    // type
    out.append([/*ET_REL*/ 1, 0]);
    // machine
    out.append([/*EM_X86_64*/ 62, 0]);
    // version
    out.append([/*EV_CURRENT*/ 1, 0, 0, 0]);
    // entry
    out.append([0, 0, 0, 0, 0, 0, 0, 0]);
    // phoff
    out.append([0, 0, 0, 0, 0, 0, 0, 0]); // no ph
    // shoff
    shoffRef = out.currentPos;
    out.append([0, 0, 0, 0, 0, 0, 0, 0]);
    // flags
    out.append([0, 0, 0, 0]);
    // ehsize
    out.append([64, 0]);
    // phentsize
    out.append([0, 0]); // no ph
    // phnum
    out.append([0, 0]);
    // shentsize
    out.append([64, 0]);
    // shnum
    shnumRef = out.currentPos;
    out.append([0, 0]);
    // shstrndx
    shstrndxRef = out.currentPos;
    out.append([0, 0]);

    // Let's write some section headers.

    out.writeUint32Le(shoffRef, out.currentPos);

    class NullTerminatedStringStore {
      #offsets;
      constructor() {
        this.#offsets = new Map();
        this.out = new BufferBuilder();
      }
      pushAndGet(str) {
        if (this.#offsets.has(str)) {
          return this.#offsets.get(str);
        }
        const offset = this.out.length;
        this.#offsets.set(str, offset);
        this.out.append(new TextEncoder("utf-8").encode(str));
        this.out.append([0]);
        return offset;
      }
    }

    const shstrs = new NullTerminatedStringStore();
    shstrs.pushAndGet("");
    shstrs.pushAndGet(".shstrtab"); // ensure that this is already present so it doesn't get added afterwards when we already got the length
    const strs = new NullTerminatedStringStore();
    strs.pushAndGet("");
    let sectionCount = 0;

    const writeSectionHeader = (name, sh) => {
      sectionCount++;
      const nameIndex = shstrs.pushAndGet(name);
      out.append([
        ...littleEndian32(nameIndex),
        ...littleEndian32(sh.type),
        ...littleEndian64(sh.flags),
        ...littleEndian64(sh.addr),
      ]);
      sectionOffsetRefs[name] = out.currentPos;
      out.append([
        ...littleEndian64(sh.offset),
        ...littleEndian64(sh.size),
        ...littleEndian32(sh.link),
        ...littleEndian32(sh.info),
        ...littleEndian64(sh.addralign),
        ...littleEndian64(sh.entsize),
      ]);
    };

    // null section
    writeSectionHeader("", {
      type: 0,
      flags: 0,
      addr: 0,
      offset: 0,
      size: 0,
      link: 0,
      info: 0,
      addralign: 0,
      entsize: 0,
    });

    // text section
    const textIndex = sectionCount;
    writeSectionHeader(".text", {
      type: /*SHT_PROGBITS*/ 1,
      flags: /*SHF_ALLOC*/ (1 << 1) | /*SHF_EXECINSTR*/ (1 << 2),
      addr: 0,
      offset: 0,
      size: textContent.length,
      link: 0,
      info: 0,
      addralign: 16,
      entsize: 0,
    });

    const rel = new BufferBuilder();
    for (const relocation of funcRelocations) {
      let idx = symbols.findIndex((sym) => sym.name === relocation.symbol);
      if (idx === -1) {
        idx = symbols.length;
        symbols.push({
          name: relocation.symbol,
          type: SYMBOL_TYPES.STT_NOTYPE,
          binding: SYMBOL_BINDING.STB_GLOBAL,
          visibility: SYMBOL_VISIBILITY.STV_DEFAULT,
          sectionIndex: 0,
          value: 0,
          size: 0,
        });
      }
      // r_offset
      rel.append([...littleEndian32(relocation.offset), ...[0, 0, 0, 0]]);
      // r_info type,sym
      rel.append(littleEndian32(relocation.kind));
      rel.append(littleEndian32(idx));
      // r_addend
      rel.append(signedLittleEndian64(relocation.addend));
    }
    const symtabIndex = sectionCount + 1;
    writeSectionHeader(".rela", {
      type: /*SHT_RELA*/ 4,
      flags: 0,
      addr: 0,
      offset: 0,
      size: rel.length,
      link: symtabIndex,
      info: textIndex,
      addralign: 8,
      entsize: 24,
    });

    const symtab = new BufferBuilder();
    const nameToSymIdx = new Map();
    let symIdx = 0;
    for (const sym of symbols) {
      const nameIdx = strs.pushAndGet(sym.name);

      symtab.append([
        ...littleEndian32(nameIdx),
        sym.type | (sym.binding << 4),
        sym.visibility,
        /*shndx*/ ...littleEndian16(sym.sectionIndex),
        /*value*/ ...littleEndian64(sym.value),
        /*size*/ ...littleEndian64(sym.size),
      ]);
      nameToSymIdx.set(sym.name, symIdx);
      symIdx++;
    }

    // symtab section
    const strTableIndex = sectionCount + 1;
    const firstGlobal = symbols.findIndex(
      (sym) => sym.binding === SYMBOL_BINDING.STB_GLOBAL
    );
    assertDefined(firstGlobal);
    writeSectionHeader(".symtab", {
      type: /*SHT_SYMTAB*/ 2,
      flags: 0,
      addr: 0,
      offset: 0,
      size: symtab.length,
      link: strTableIndex,
      info: firstGlobal,
      addralign: 8,
      entsize: 24,
    });

    // strtab section
    writeSectionHeader(".strtab", {
      type: /*SHT_STRTAB*/ 3,
      flags: 0,
      addr: 0,
      offset: 0,
      size: strs.out.length,
      link: 0,
      info: 0,
      addralign: 1,
      entsize: 0,
    });

    out.writeUint16Le(shstrndxRef, sectionCount);

    const totalSectionCount = sectionCount + 1;
    out.writeUint16Le(shnumRef, totalSectionCount);

    // shstrtab section
    writeSectionHeader(".shstrtab", {
      type: /*SHT_STRTAB*/ 3,
      flags: 0,
      addr: 0,
      offset: 0,
      size: shstrs.out.length,
      link: 0,
      info: 0,
      addralign: 1,
      entsize: 0,
    });

    const patch32 = (baseOffset, value) => {
      assertDefined(baseOffset, value);
      out.writeUint32Le(baseOffset, value);
    };

    alignTo(out, 16);
    patch32(sectionOffsetRefs[".text"], out.currentPos);
    out.append(textContent);

    alignTo(out, 8);
    patch32(sectionOffsetRefs[".rela"], out.currentPos);
    out.append(rel.toUint8Array());

    patch32(sectionOffsetRefs[".strtab"], out.currentPos);
    out.append(strs.out.toUint8Array());

    alignTo(out, 8);
    patch32(sectionOffsetRefs[".symtab"], out.currentPos);
    out.append(symtab.toUint8Array());

    patch32(sectionOffsetRefs[".shstrtab"], out.currentPos);
    out.append(shstrs.out.toUint8Array());

    return out.toUint8Array();
  }

  const funcs = [];

  for (const func of ast) {
    const ib = codegenFunction(func);
    funcs.push({
      name: func.name.ident,
      code: ib.out,
      relocations: ib.relocations,
    });
  }

  console.dir(funcs, { depth: 5 });

  const obj = generateObjectFile(funcs);

  return obj;
}

async function link(object) {
  async function execWithForwardedOutput(command, args) {
    return new Promise((resolve, reject) => {
      const child = spawn(command, args, {
        stdio: "inherit",
      });
      child.on("close", (code) => {
        if (code === 0) {
          resolve();
        } else {
          reject(new CompilerError("gcc failed to link", 0));
        }
      });
    });
  }

  // we could use a temporary directory in the future, but let's keep this debuggable for now
  const outputFile = "output.o";
  fs.writeFile(outputFile, object);

  await execWithForwardedOutput("gcc", ["-Wl,-znoexecstack", outputFile]);
  await execWithForwardedOutput("gdb", [
    "--batch",
    "--command",
    "dump-main.gdb",
    "a.out",
  ]);
}

async function compile(input) {
  const tokens = lex(input);
  console.log("tokens", tokens);
  const ast = parse(tokens);
  console.log("ast");
  console.dir(ast, { depth: 20 });
  const object = lower(ast);

  return link(object);
}

const fileName = process.argv[2];
const input = await fs.readFile(fileName, "utf-8");
console.log("input", input);

try {
  await compile(input);
} catch (e) {
  if (e instanceof CompilerError) {
    console.error(e.render(fileName, input));
  } else {
    throw e;
  }
}

function assert(condition) {
  if (!condition) {
    throw new Error("assertion failed");
  }
}

function assertDefined(...values) {
  values.forEach((value, i) => {
    if (value === null || value === undefined) {
      throw new Error(`assertion failed, argument ${i} undefined or nu ll`);
    }
  });
}
