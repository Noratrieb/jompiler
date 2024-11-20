import fs from "node:fs/promises";

// https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
// yep.

class CompilerError extends Error {
  constructor(message, span) {
    super(message);
    if (!span) {
      throw new Error("span must be present");
    }
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
  function alphabetic(char) {
    return (char >= "a" && char <= "z") || (char >= "A" && char <= "Z");
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
    } else if (alphabetic(head)) {
      const span = i - 1;
      let ident = head;
      while (alphabetic(input[i])) {
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
        case ";": {
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

    const parseLogicalAnd = generateBinaryParser(["&&"], parsePostfix);
    const parseLogicalOr = generateBinaryParser(["||"], parseLogicalAnd);
    const parseAssignment = generateBinaryParser(
      ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", , "^=", "|="],
      parseLogicalOr
    );
    const parseComma = generateBinaryParser([","], parseAssignment);

    return parseComma(tok);
  }

  function parseBlock(tok) {
    tok.expect("{", "start of block");

    const statements = [];

    while (tok.peek()?.kind !== "}") {
      // TODO: non-expression statements
      const expr = parseExpr(tok);
      statements.push({
        kind: "expr",
        expr,
        span: expr.span,
      });
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
      ret,
      name,
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
  */

  function littleEndian32(number) {
    return [
      number & 0xf,
      (number >> 8) & 0xf,
      (number >> 16) & 0xf,
      (number >> 24) & 0xf,
    ];
  }

  const MOD_REG = 0b11;

  const RM_A = 0b000;
  const RM_C = 0b001;

  const REG_A = RM_A;
  const REG_C = RM_C;

  const REG_IGNORED = 0;

  function modRm(mod, rm, reg) {
    return (mod >> 6) | rm | (reg << 3);
  }

  class InstBuilder {
    #out;
    #stackSize;
    constructor() {
      this.#out = new Uint8Array();
      this.#stackSize = 0;
    }

    reserveStack(size) {
      this.#stackSize += size;
      return this.#stackSize;
    }

    movEaxImm32(imm) {
      // mov eax, imm
      this.#append([
        0xc7,
        modRm(MOD_REG, RM_A, REG_IGNORED),
        ...littleEndian32(imm),
      ]);
    }

    #append(code) {
      this.#out += code;
    }
  }

  function codegenExpr(ib, expr) {
    switch (expr.kind) {
      case "call": {
        if (expr.lhs.kind !== "ident") {
          throw new Error("bad");
        }
        if (expr.args.length !== 1) {
          throw new Error("bad");
        }

        const arg0 = codegenExpr(ib, expr.args[0]);

        break;
      }
      case "integer": {
        ib.movEaxImm32(expr.integer);
        break;
      }
      case "+": {
      }
      default: {
        throw new Error(`unsupported expr: ${expr.kind}`);
      }
    }
  }

  function codegenFunction(func) {
    const ib = new InstBuilder();

    for (const stmt of func.body) {
      console.log("doing cg for", stmt.kind);

      switch (stmt.kind) {
        case "expr": {
          codegenExpr(ib, stmt.expr);
          break;
        }
        default: {
          throw new Error(`unsupported stmt: ${stmt.kind}`);
        }
      }
    }

    return ib;
  }

  for (const func of ast) {
    const ib = codegenFunction(func);
  }
}

function compile(input) {
  const tokens = lex(input);
  console.log(tokens);
  const ast = parse(tokens);
  console.dir(ast, { depth: 20 });
  lower(ast);
}

const fileName = process.argv[2];
const input = await fs.readFile(fileName, "utf-8");
console.log(input);

try {
  compile(input);
} catch (e) {
  if (e instanceof CompilerError) {
    console.error(e.render(fileName, input));
  } else {
    throw e;
  }
}
