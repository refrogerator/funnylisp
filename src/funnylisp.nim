import print
import std/tables
from std/strutils import parseFloat, parseInt, join
from std/sequtils import map, mapIt
import std/strformat
import std/cmdline

type
  NodeKind = enum
    nkInt,
    nkFloat,
    nkString,
    nkSymbol,
    nkList,
    nkBuiltin,
    nkBuiltinMacro,
    nkBool,
    nkLambda,
    nkCons,
    nkNone
  Node = ref NodeObj
  Func = object
    args: seq[string]
    body: seq[Node]
  SymbolStack = seq[ref Table[string, Node]]
  Cons = object
    left: Node
    right: Node
  NodeObj = object
    case kind: NodeKind
    of nkInt: intVal: int
    of nkFloat: floatVal: float
    of nkString: strVal: string
    of nkSymbol: symVal: string
    of nkList: listVal: seq[Node]
    of nkBool: boolVal: bool
    of nkBuiltin: builtinVal: proc(n: seq[Node]): Node
    of nkBuiltinMacro: builtinMacroVal: proc(n: seq[Node], locals: var SymbolStack): Node
    of nkLambda: lambdaVal: Func
    of nkCons: consVal: Cons
    of nkNone: none: int

proc intNode(val: int): Node =
  Node(kind: nkInt, intVal: val)

proc floatNode(val: float): Node =
  Node(kind: nkFloat, floatVal: val)

proc listNode(val: seq[Node]): Node =
  Node(kind: nkList, listVal: val)

proc builtinNode(val: proc(n: seq[Node]): Node): Node =
  Node(kind: nkBuiltin, builtinVal: val)

proc builtinMacroNode(val: proc(n: seq[Node], locals: var SymbolStack): Node): Node =
  Node(kind: nkBuiltinMacro, builtinMacroVal: val)

proc stringNode(val: string): Node =
  Node(kind: nkString, strVal: val)

proc symbolNode(val: string): Node =
  Node(kind: nkSymbol, symVal: val)

proc boolNode(val: bool): Node =
  Node(kind: nkBool, boolVal: val)

proc lambdaNode(args: seq[string], body: seq[Node]): Node =
  Node(kind: nkLambda, lambdaVal: Func(args: args, body: body))

proc consNode(left: Node, right: Node): Node =
  Node(kind: nkCons, consVal: Cons(left: left, right: right))

proc noneNode(): Node =
  Node(kind: nkNone)

type
  SymbolNotFound* = object of ValueError
  TypeMismatch* = object of ValueError
  ArityMismatch* = object of ValueError

proc findLocal(stack: var SymbolStack, symbol: string): Node =
  for i in countdown(stack.len - 1, 0):
    if stack[i].contains(symbol):
      return stack[i][symbol]
  raise newException(SymbolNotFound, fmt"symbol {symbol} not found")

proc findSymbol(symbol: string, globals: ref Table[string, Node], locals: var SymbolStack): Node =
  try: findLocal(locals, symbol)
  except SymbolNotFound:
    if globals.contains(symbol):
      globals[symbol]
    else:
      raise newException(SymbolNotFound, fmt"symbol {symbol} not found")

proc readTemp(temp: string, quote: bool): Node =
  # try: Node(kind: nkInt, intVal: parseInt(temp))
  # except:
  let res = try: floatNode(parseFloat(temp))
            except: symbolNode(temp)
  if quote:
    listNode(@[symbolNode("quote"), res])
  else:
    res

proc read(s: string): seq[Node] =
  var listStack: seq[seq[Node]] = @[]
  var listQuoteStack: seq[bool] = @[]
  var res: seq[Node] = @[]
  var temp = ""
  var isString = false
  var quote = false
  for c in s:
    if isString:
      if c == '"':
        if listStack.len() > 0:
          listStack[^1].add(stringNode(temp))
        else:
          res.add(stringNode(temp))
        quote = false
        reset temp
        isString = false
      else:
        temp.add(c)
    else:
      case c:
        of '\'': # TODO: add reader macros
          quote = true
        of '(':
          listQuoteStack.add(quote)
          quote = false

          if temp.len() != 0:
            if listStack.len() > 0:
              listStack[^1].add(readTemp(temp, quote))
            else:
              res.add(readTemp(temp, quote))
            quote = false
            reset temp

          listStack.add(@[])
        of ')':
          if temp.len() != 0:
            listStack[^1].add(readTemp(temp, quote))
            quote = false
            reset temp
          var tempL = listStack.pop()
          if listQuoteStack.pop():
            tempL.insert(symbolNode("quote"), 0)
          if listStack.len() > 1:
            listStack[^2].add(listNode(tempL))
          elif listStack.len() == 1:
            listStack[0].add(listNode(tempL))
          else:
            res.add(listNode(tempL))
        of '"':
          isString = true
          if temp.len() != 0:
            listStack[^1].add(readTemp(temp, quote))
            quote = false
            reset temp
        of ' ':
          if temp.len() != 0:
            listStack[^1].add(readTemp(temp, quote))
            quote = false
            reset temp
        of '\n':
          continue
        else:
          temp.add(c)
  if temp.len() != 0:
    res.add(readTemp(temp, quote))
    quote = false
    reset temp
  res

proc evalL(nodes: seq[Node], globals: ref Table[string, Node], locals: var SymbolStack): Node =
  var lastEval = noneNode()
  for node in nodes:
    case node.kind:
      of nkList:
        let j =
          case node.listVal[0].kind:
            of nkSymbol:
              node.listVal[0].symVal
            else:
              raise newException(ValueError, "Cant call non-function")
        let f = findSymbol(j, globals, locals)
        case f.kind:
          of nkBuiltin:
            var args: seq[Node] = @[]
            for arg in node.listVal[1..^1]:
              args.add(evalL(@[arg], globals, locals))
            lastEval = f.builtinVal(args)
          of nkBuiltinMacro:
            lastEval = f.builtinMacroVal(node.listVal[1..^1], locals)
          of nkLambda:
            var newLocals = newTable[string, Node]()
            for i, arg in f.lambdaVal.args:
              newLocals[arg] = node.listVal[i + 1]
            locals.add(newLocals)
            lastEval = evalL(f.lambdaVal.body, globals, locals)
            discard locals.pop()
          else:
            raise newException(ValueError, "Cant call non-function")
      of nkSymbol:
        lastEval = findSymbol(node.symVal, globals, locals)
      else:
        lastEval = node

  lastEval

proc printL(nodes: seq[Node]) =
  for node in nodes:
    case node.kind:
      of nkLambda:
        stdout.write "(lambda ("
        stdout.write join(node.lambdaVal.args, " ")
        stdout.write "))"
      of nkBool:
        if node.boolVal:
          stdout.write "t"
        else:
          stdout.write "f"
      of nkNone:
        stdout.write "none"
      of nkSymbol:
        stdout.write node.symVal
      of nkInt:
        stdout.write $node.intVal
      of nkFloat:
        stdout.write $node.floatVal
      of nkList:
        stdout.write '('
        printL node.listVal
        stdout.write ')'
      of nkBuiltin:
        stdout.write "<builtin function>"
      of nkBuiltinMacro:
        stdout.write "<builtin macro>"
      of nkString:
        stdout.write node.strVal
      of nkCons:
        stdout.write '('
        printL @[node.consVal.left]
        stdout.write " . "
        printL @[node.consVal.right]
        stdout.write ')'

proc pprintL(nodes: seq[Node]) =
  var first = true
  for node in nodes:
    if first:
      first = false
    else:
      stdout.write ' '
    case node.kind:
      of nkLambda:
        stdout.write "(lambda ("
        stdout.write join(node.lambdaVal.args, " ")
        stdout.write "))"
      of nkBool:
        if node.boolVal:
          stdout.write "t"
        else:
          stdout.write "f"
      of nkNone:
        stdout.write "none"
      of nkSymbol:
        stdout.write node.symVal
      of nkInt:
        stdout.write $node.intVal
      of nkFloat:
        stdout.write $node.floatVal
      of nkList:
        stdout.write '('
        pprintL node.listVal
        stdout.write ')'
      of nkBuiltin:
        stdout.write "<builtin function>"
      of nkBuiltinMacro:
        stdout.write "<builtin macro>"
      of nkString:
        stdout.write '"'
        stdout.write node.strVal
        stdout.write '"'
      of nkCons:
        stdout.write '('
        pprintL @[node.consVal.left]
        stdout.write " . "
        pprintL @[node.consVal.right]
        stdout.write ')'

var globals = newTable[string, Node]()

proc asNumber(node: Node): float =
  case node.kind:
    of nkFloat:
      node.floatVal
    of nkInt:
      float(node.intVal)
    else:
      raise newException(TypeMismatch, fmt"type {node.kind} is not a number")

proc asString(node: Node): string =
  case node.kind:
    of nkString:
      node.strVal
    else:
      raise newException(TypeMismatch, fmt"type {node.kind} is not a string")

proc asSymbol(node: Node): string =
  case node.kind:
    of nkSymbol:
      node.symVal
    else:
      raise newException(TypeMismatch, fmt"type {node.kind} is not a symbol")

proc asCons(node: Node): Cons =
  case node.kind:
    of nkCons:
      node.consVal
    else:
      raise newException(TypeMismatch, fmt"type {node.kind} is not a pair")

proc asList(node: Node): seq[Node] =
  case node.kind:
    of nkList:
      node.listVal
    else:
      raise newException(TypeMismatch, fmt"type {node.kind} is not a list")

globals["define"] = builtinMacroNode(proc(a: seq[Node], locals: var SymbolStack): Node =
    if a.len() != 2:
      raise newException(ArityMismatch, fmt"define called with {a.len()} arguments but needs 2")

    # print a[1]
    globals[asSymbol(a[0])] = evalL(@[a[1]], globals, locals)
    noneNode()
)

globals["quote"] = builtinMacroNode(proc(a: seq[Node], locals: var SymbolStack): Node =
    if a.len() == 0:
      raise newException(ArityMismatch, fmt"quote has to be called with at least 1 argument")
    elif a.len() == 1:
      a[0]
    else:
      listNode(a)
)

globals["vector"] = builtinMacroNode(proc(a: seq[Node], locals: var SymbolStack): Node =
    listNode(a)
)

globals["cons"] = builtinNode(proc(a: seq[Node]): Node =
    if a.len() != 2:
      raise newException(ArityMismatch, fmt"cons has to be called with 2 arguments")
    else:
      consNode(a[0], a[1])
)

globals["car"] = builtinNode(proc(a: seq[Node]): Node =
    if a.len() != 1:
      raise newException(ArityMismatch, fmt"car has to be called with 1 argument")
    else:
      asCons(a[0]).left
)

globals["cdr"] = builtinNode(proc(a: seq[Node]): Node =
    if a.len() != 1:
      raise newException(ArityMismatch, fmt"cdr has to be called with 1 argument")
    else:
      asCons(a[0]).right
)

globals["left"] = globals["car"]
globals["right"] = globals["cdr"]

globals["vector-ref"] = builtinNode(proc(a: seq[Node]): Node =
    if a.len() != 2:
      raise newException(ArityMismatch, fmt"vector-ref has to be called with 2 arguments")
    else:
      asList(a[0])[int(asNumber(a[1]))]
)

# globals["vector-set!"] = builtinNode(proc(a: seq[Node]): Node =
#     if a.len() != 3:
#       raise newException(ArityMismatch, fmt"vector-set! has to be called with 3 arguments")
#     else:
#       asList(a[0])[int(asNumber(a[1]))].set(a[2])
# )

globals["lambda"] = builtinMacroNode(proc(a: seq[Node], locals: var SymbolStack): Node =
    if a.len() < 2:
      raise newException(ArityMismatch, fmt"lambda has to be called with at least 2 arguments")
    else:
      lambdaNode(map(asList(a[0]), asSymbol), a[1..^1])
)

globals["if"] = builtinMacroNode(proc(a: seq[Node], locals: var SymbolStack): Node =
  if a.len() != 3:
    raise newException(ArityMismatch, fmt"if has to be called with 3 arguments")
  let v = evalL(@[a[0]], globals, locals)
  case v.kind:
    of nkBool:
      if v.boolVal:
        evalL(@[a[1]], globals, locals)
      else:
        evalL(@[a[2]], globals, locals)
    else:
      raise newException(TypeMismatch, fmt"the first argument to if has to be a bool")
)

# syms["case"] = builtinMacroNode(proc(a: seq[Node]): Node =
#   let v = syms[asSymbol(a[0])]
# )

globals["+"] = builtinNode(proc(a: seq[Node]): Node =
    var sum = 0.0
    for node in a:
      sum += asNumber(node)
    floatNode(sum)
)

globals["-"] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      res -= asNumber(node)
    floatNode(res)
)

globals["*"] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      res *= asNumber(node)
    floatNode(res)
)

globals["/"] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      res /= asNumber(node)
    floatNode(res)
)

globals[">"] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      if res <= asNumber(node):
        return boolNode(false)
    boolNode(true)
)

globals["<"] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      if res >= asNumber(node):
        return boolNode(false)
    boolNode(true)
)

globals[">="] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      if res < asNumber(node):
        return boolNode(false)
    boolNode(true)
)

globals["<="] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      if res > asNumber(node):
        return boolNode(false)
    boolNode(true)
)

globals["="] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      if res != asNumber(node):
        return boolNode(false)
    boolNode(true)
)

globals["readline"] = builtinNode(proc(a: seq[Node]): Node =
    stringNode(stdin.readLine())
)

globals["readfile"] = builtinNode(proc(a: seq[Node]): Node =
    stringNode(readFile(asString(a[0])))
)

globals["read"] = builtinNode(proc(a: seq[Node]): Node =
    if a.len() != 1:
      raise newException(ArityMismatch, "read has to be called with 1 argument")
    let res = read(asString(a[0]))
    if res.len() == 1:
      res[0]
    else:
      listNode(res)
)

globals["eval"] = builtinMacroNode(proc(a: seq[Node], locals: var SymbolStack): Node =
    if a.len() != 1:
      raise newException(ArityMismatch, "read has to be called with 1 argument")
    evalL(asList(evalL(@[a[0]], globals, locals)), globals, locals)
)

globals["write"] = builtinNode(proc(a: seq[Node]): Node =
    printL(a)
    noneNode()
)

globals["writeln"] = builtinNode(proc(a: seq[Node]): Node =
    printL(a)
    echo ""
    noneNode()
)

globals["print"] = builtinNode(proc(a: seq[Node]): Node =
    pprintL(a)
    noneNode()
)

globals["println"] = builtinNode(proc(a: seq[Node]): Node =
    pprintL(a)
    echo ""
    noneNode()
)

globals["t"] = boolNode(true)
globals["f"] = boolNode(false)

var locals: SymbolStack = @[]

# discard evalL(read "(define add (lambda (a b) (+ a b)))", globals, locals)

case commandLineParams()[0]:
  of "run":
    discard evalL(read readFile commandLineParams()[1], globals, locals)
  of "repl":
    while true:
      stdout.write "lol> "

      try:
        pprintL @[evalL(read readLine stdin, globals, locals)]
        # printL read readLine stdin
        # print read readLine stdin
        echo ""
      except EOFError:
        echo ""
        echo "goodbye"
        quit 1
