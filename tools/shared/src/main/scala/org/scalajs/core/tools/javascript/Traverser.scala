package org.scalajs.core.tools.javascript

import Trees._

class Traverser {
  def traverse(_tree: Tree): Unit = _tree match {
    case Block(trees) => trees foreach traverse
    case VarDef(_, tree) => traverse(tree)
    case Labeled(_, tree) => traverse(tree)
    case Assign(left, right) =>
      traverse(left)
      traverse(right)
    case Return(tree) => traverse(tree)
    case If(cond, t, f) =>
      traverse(cond)
      traverse(t)
      traverse(f)
    case While(cond, tree, _) =>
      traverse(cond)
      traverse(tree)
    case DoWhile(tree, cond, _) =>
      traverse(tree)
      traverse(cond)
    case Try(tree, _, cat, fin) =>
      traverse(tree)
      traverse(cat)
      traverse(fin)
    case Throw(tree) => traverse(tree)
    case Switch(selector, cases, default) =>
      traverse(selector)
      cases.foreach {
        case (a, b) =>
          traverse(a)
          traverse(b)
      }
      traverse(default)
    case New(tree, args) =>
      traverse(tree)
      args foreach traverse
    case DotSelect(tree, _) => traverse(tree)
    case BracketSelect(tree, item) =>
      traverse(tree)
      traverse(item)
    case Apply(tree, args) =>
      traverse(tree)
      args foreach traverse
    case Delete(tree) => traverse(tree)
    case UnaryOp(_, tree) => traverse(tree)
    case BinaryOp(_, l, r) =>
      traverse(l)
      traverse(r)
    case ArrayConstr(trees) => trees foreach traverse
    case ObjectConstr(fields) => fields.map(_._2) foreach traverse
    case Function(_, tree) => traverse(tree)

    case EmptyTree | _:DocComment | _:ParamDef | _:Skip | _:Break | _:Continue |
         _:Debugger | _:Literal | _:VarRef | _:This =>
  }
}
