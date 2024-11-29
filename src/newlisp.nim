import print
import std/tables
from std/strutils import parseFloat, parseInt
import std/strformat

type
  NodeKind = enum
    nkInt,
    nkFloat,
    nkString,
    nkSymbol,
    nkList,
    nkBuiltin,
    nkNone
  Node = ref NodeObj
  NodeObj = object
    case kind: NodeKind
    of nkInt: intVal: int
    of nkFloat: floatVal: float
    of nkString: strVal: string
    of nkSymbol: symVal: string
    of nkList: listVal: seq[Node]
    of nkBuiltin: builtinVal: proc(n: seq[Node]): Node
    of nkNone: none: int

proc intNode(val: int): Node =
  Node(kind: nkInt, intVal: val)

proc floatNode(val: float): Node =
  Node(kind: nkFloat, floatVal: val)

proc listNode(val: seq[Node]): Node =
  Node(kind: nkList, listVal: val)

proc builtinNode(val: proc(n: seq[Node]): Node): Node =
  Node(kind: nkBuiltin, builtinVal: val)

proc stringNode(val: string): Node =
  Node(kind: nkString, strVal: val)

proc symbolNode(val: string): Node =
  Node(kind: nkSymbol, symVal: val)

proc noneNode(): Node =
  Node(kind: nkNone)

proc readTemp(temp: string): Node =
  # try: Node(kind: nkInt, intVal: parseInt(temp))
  # except:
  try: floatNode(parseFloat(temp))
  except: symbolNode(temp)

proc read(s: string): seq[Node] =
  var listStack: seq[seq[Node]] = @[]
  var res: seq[Node] = @[]
  var temp = ""
  var isString = false
  for c in s:
    if isString:
      if c == '"':
        listStack[^1].add(stringNode(temp))
        reset temp
        isString = false
      else:
        temp.add(c)
    else:
      case c:
        of '(':
          if temp.len() != 0:
            listStack[^1].add(readTemp(temp))
            reset temp
          listStack.add(@[])
        of ')':
          if temp.len() != 0:
            listStack[^1].add(readTemp(temp))
            reset temp
          if listStack.len() > 1:
            listStack[^2].add(listNode(listStack.pop()))
          else:
            res.add(Node(kind: nkList, listVal: listStack.pop()))
        of '"':
          isString = true
          if temp.len() != 0:
            listStack[^1].add(readTemp(temp))
            reset temp
        of ' ':
          if temp.len() != 0:
            listStack[^1].add(readTemp(temp))
            reset temp
        else:
          temp.add(c)
  if temp.len() != 0:
    res.add(readTemp(temp))
    reset temp
  res

proc evalL(nodes: seq[Node], symbols: ref Table[string, Node]): Node =
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
        let f = symbols[j]
        case f.kind:
          of nkBuiltin:
            var args: seq[Node] = @[]
            for arg in node.listVal[1..^1]:
              args.add(evalL(@[arg], symbols))
            lastEval = f.builtinVal(args)
          else:
            raise newException(ValueError, "Cant call non-function")
      else:
        lastEval = node

  lastEval

proc printL(nodes: seq[Node]) =
  var first = true
  for node in nodes:
    if first:
      first = false
    else:
      stdout.write ' '
    case node.kind:
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
        stdout.write "<builtin>"
      of nkString:
        stdout.write '"'
        stdout.write node.strVal
        stdout.write '"'

type
  TypeMismatch* = object of ValueError

var syms = newTable[string, Node]()

proc asNumber(node: Node): float =
  case node.kind:
    of nkFloat:
      node.floatVal
    of nkInt:
      float(node.intVal)
    else:
      raise newException(TypeMismatch, fmt"type {node.kind} is not a number")

syms["+"] = builtinNode(proc(a: seq[Node]): Node =
    var sum = 0.0
    for node in a:
      sum += asNumber(node)
    floatNode(sum)
)

syms["-"] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      res -= asNumber(node)
    floatNode(res)
)

syms["*"] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      res *= asNumber(node)
    floatNode(res)
)

syms["/"] = builtinNode(proc(a: seq[Node]): Node =
    var res = asNumber(a[0])
    for node in a[1..^1]:
      res /= asNumber(node)
    floatNode(res)
)

syms["print"] = builtinNode(proc(a: seq[Node]): Node =
    printL(a)
    noneNode()
)

syms["println"] = builtinNode(proc(a: seq[Node]): Node =
    printL(a)
    echo ""
    noneNode()
)

while true:
  stdout.write "lol> "

  try:
    printL @[evalL(read readLine stdin, syms)]
    # print read readLine stdin
    echo ""
  except EOFError:
    echo ""
    echo "goodbye"
    quit 1
