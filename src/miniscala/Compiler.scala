package miniscala

import miniscala.AbstractMachine._
import miniscala.Ast._

object Compiler {

  def compile(e: Exp): Executable = {

    case class IdDesc(x: Id, mutable: Boolean)

    val Unit = Const(0)

    def lookup(x: Id, idstack: List[IdDesc]): (IdIndex, Boolean) = {
      // find the position of identifier x in idstack
      val index = idstack.indexWhere(p => p.x == x)
      if (index == -1) throw new Exception(s"$x not found")
      // return the position and a boolean flag that indicates whether the identifier was declared with 'var'
      (index, idstack(index).mutable)
    }

    def compileFun(params: List[FunParam], body: Exp, freeids: List[Id], defs: List[DefDecl], idstack: List[IdDesc]): List[Instruction] = {
      // prepare the new idstack for the function body, with an entry for each free non-def identifier, each def, and each parameter
      val defids = defs.map(d => d.fun).toSet
      val freenondefs = freeids.filterNot(defids.contains)
      val freeidsstack = freenondefs.map(x => IdDesc(x, lookup(x, idstack)._2))
      val defsstack = defs.map(d => IdDesc(d.fun, mutable = false))
      val paramsstack = params.map(p => IdDesc(p.x, mutable = false))
      // compile the function body
      val bodycode = compile(body, paramsstack ++ freeidsstack ++ defsstack, tailpos = true) ++ List(Return)
      // find idstack index for each free identifier (excluding defs in same block)
      val indices = freenondefs.map(x => lookup(x, idstack)._1)
      // produce a Lambda instruction
      List(AbstractMachine.Lambda(indices, bodycode))
    }

    def compile(e: Exp, idstack: List[IdDesc], tailpos: Boolean): List[Instruction] =
      e match {
        case IntLit(c) =>
          List(Const(c))
        case BoolLit(c) =>
          if (c) List(Const(1)) else List(Const(0))
        case BinOpExp(leftexp, op, rightexp) =>
          compile(leftexp, idstack, tailpos) ++ compile(rightexp, idstack, tailpos) ++ List(op match {
            case PlusBinOp() => Add
            case MinusBinOp() => Sub
            case MultBinOp() => Mul
            case DivBinOp() => Div
            case EqualBinOp() => Eq
            case LessThanBinOp() => Lt
            case LessThanOrEqualBinOp() => Leq
            case AndBinOp() => And
            case OrBinOp() => Or
            case _ => throw new CompilerError(e)
          })
        case UnOpExp(op, exp) =>
          compile(exp, idstack, tailpos) ++ List(op match {
            case NegUnOp() => Neg
            case NotUnOp() => Not
          })
        case IfThenElseExp(condexp, thenexp, elseexp) =>
          compile(condexp, idstack, tailpos) ++ List(Branch(compile(thenexp, idstack, tailpos), compile(elseexp, idstack, tailpos)))
        case WhileExp(cond, body) =>
          compile(cond, idstack, tailpos) ++ List(Loop(compile(cond, idstack, tailpos), compile(body, idstack, tailpos)))
//          List(Loop(compile(cond, idstack, tailpos), compile(body, idstack, tailpos)), Unit)
        case BlockExp(vals, vars, defs, Nil, exps) =>
          var inst = List[Instruction]()
          var ids = idstack
          for (v <- vals) {
            inst = inst ++ compile(v.exp, ids, tailpos = false) ++ List(EnterScope)
            ids = IdDesc(v.x, mutable = false) :: ids
          }

          for (v <- vars) {
            inst = inst ++ List(Alloc, Dup) ++ compile(v.exp, ids, tailpos = false) ++ List(Store, EnterScope)
            ids = IdDesc(v.x, mutable = true) :: ids
          }

          for (d <- defs) {
            inst = inst ++ compileFun(d.params, d.body, ids.map(p => p.x), defs, ids)
            ids = IdDesc(d.fun, mutable = false) :: ids
          }
          inst = inst ++ List(EnterScopeDefs(defs.length))

          for (i <- 0 to exps.size - 2) {
            inst = inst ++ compile(exps(i), ids, tailpos = false)
          }
          (inst ++ compile(exps.last, ids, tailpos = true)) ++ List(ExitScope(ids.length - idstack.length))
        case VarExp(x) =>
          val (index, isVar) = lookup(x, idstack)
          if (isVar)
            List(Read(index), Load)
          else
            List(Read(index))
        case AssignmentExp(x, exp) =>
          val index = lookup(x, idstack)._1
          List(Read(index)) ++ compile(exp, idstack, tailpos) ++ List(Store)
        case LambdaExp(params, body) =>
          compileFun(params, body, Vars.freeVars(e).toList.sorted, Nil, idstack)
        case CallExp(funexp, args) =>
          // compile funexp and args, and then add a Call instruction
          compile(funexp, idstack, tailpos) ++ args.flatMap(arg => compile(arg, idstack, tailpos)) ++ List(Call(args.length, tailpos))        case _ => throw new CompilerError(e)
      }

    val freeids = Vars.freeVars(e).toList.sorted
    Executable(freeids, compile(e, freeids.map(x => IdDesc(x, mutable = false)), tailpos = false))
  }

  class CompilerError(node: AstNode) extends MiniScalaError(s"Sorry, I don't know how to compile $node", node.pos)
}
