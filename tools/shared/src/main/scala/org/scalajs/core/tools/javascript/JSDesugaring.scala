/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.javascript

import scala.language.implicitConversions

import scala.annotation.{switch, tailrec}

import org.scalajs.core.ir
import ir._
import ir.Position._
import ir.Transformers._
import ir.Trees._
import ir.Types._

import org.scalajs.core.tools.sem._
import CheckedBehavior._

import org.scalajs.core.tools.javascript.{Trees => js}

import java.io.StringWriter

/** Desugaring of the IR to JavaScript.
 *
 *  The general shape and compliance to standards is chosen with an
 *  [[OutputMode]].
 *
 *  The major difference between the IR and JS is that most constructs can be
 *  used in expression position. The main work of the desugaring is to
 *  unnest complex constructs in expression position so that they become
 *  statements.
 *
 *  The general idea is two-folded:
 *  1) Unnest complex constructs in "argument position":
 *     When a complex construct is used in a non-rhs expression position
 *     (argument to a function, operand, condition of an if, etc.), that we
 *     call "argument position", declare a variable before the statement,
 *     assign the complex construct to it and then use that variable in the
 *     argument position instead.
 *  2) Push LHS's inside complex RHS's:
 *     When an rhs is a complex construct, push the lhs inside the complex
 *     construct. Are considered lhs:
 *     * Assign, i.e., `x =`
 *     * VarDef, i.e., `var x =`
 *     * Return, i.e., `return`
 *     * (EmptyTree is also used as a trick for code reuse)
 *     In fact, think that, in this context, LHS means: what to do with the
 *     result of evaluating the RHS.
 *
 *  --------------------------------------------------------------------------
 *
 *  Typical example, consider the method call:
 *
 *  obj.meth({
 *    var x = foo(42);
 *    x*x
 *  });
 *
 *  According to rule 1), the block that is passed as a parameter to obj.meth
 *  is first extracted in a synthetic var:
 *
 *  var x\$1 = {
 *    var x = foo(42);
 *    x*x
 *  }
 *  obj.meth(x\$1);
 *
 *  Then, according to rule 2), the lhs `var x\$1 =` is pushed inside the block:
 *
 *  {
 *    var x = foo(42);
 *    var x\$1 = x*x;
 *  }
 *  obj.meth(x\$1);
 *
 *  Because bare blocks are non-significant in JS, this is equivalent to
 *
 *  var x = foo(42);
 *  var x\$1 = x*x;
 *  obj.meth(x\$1);
 *
 *  --------------------------------------------------------------------------
 *
 *  JSDesugaring does all this in a single pass, but it helps to think that:
 *  * Rule 1) is implemented by unnest(), and used most notably in
 *    * transformStat() for statement-only constructs
 *    * pushLhsInto() for statement-or-expression constructs
 *  * Rule 2) is implemented by pushLhsInto()
 *  * Emitting the class structure is delegated to [[ScalaJSClassEmitter]].
 *
 *  There are a few other things that JSDesugaring takes care of:
 *  * Transform Scala expressions into their JS equivalent, taking the
 *    Scala.js class encoding into account.
 *  * And tiny details.
 *
 *  @author Sébastien Doeraene
 */
object JSDesugaring {

  private final val ScalaJSEnvironmentName = "ScalaJS"

  /** Desugars a statement of the IR into ES5 JavaScript. */
  @deprecated("Use an overload with an explicit OutputMode.", "0.6.2")
  def desugarJavaScript(tree: Tree, semantics: Semantics): js.Tree =
    desugarJavaScript(tree, semantics, OutputMode.ECMAScript51Global)

  /** Desugars a statement of the IR into a given mode of JavaScript. */
  def desugarJavaScript(tree: Tree, semantics: Semantics,
      outputMode: OutputMode): js.Tree =
    desugarJavaScript(tree, semantics, outputMode, Env.empty)

  /** Desugars a statement of the IR into ES5 JavaScript under a
   *  given environment.
   */
  @deprecated("Use an overload with an explicit OutputMode.", "0.6.2")
  def desugarJavaScript(tree: Tree, semantics: Semantics, env: Env): js.Tree =
    desugarJavaScript(tree, semantics, OutputMode.ECMAScript51Global, env)

  /** Desugars a statement of the IR into a given mode of JavaScript under a
   *  given environment.
   */
  def desugarJavaScript(tree: Tree, semantics: Semantics,
      outputMode: OutputMode, env: Env): js.Tree = {
    val desugar = new JSDesugar(semantics, outputMode)
    try {
      desugar.transformStat(tree)(env)
    } catch {
      case cause: Throwable =>
        throw new DesugarException(tree, cause)
    }
  }

  private[javascript] implicit def transformIdent(ident: Ident): js.Ident =
    js.Ident(ident.name, ident.originalName)(ident.pos)

  private[javascript] def transformParamDef(paramDef: ParamDef): js.ParamDef =
    js.ParamDef(paramDef.name)(paramDef.pos)

  private class JSDesugar(semantics: Semantics, outputMode: OutputMode) {

    private implicit def implicitOutputMode: OutputMode = outputMode

    // Synthetic variables

    var syntheticVarCounter: Int = 0

    def newSyntheticVar()(implicit pos: Position): Ident = {
      syntheticVarCounter += 1
      Ident("jsx$" + syntheticVarCounter, None)
    }

    def resetSyntheticVarCounterIn[A](f: => A): A = {
      val savedCounter = syntheticVarCounter
      syntheticVarCounter = 0
      try f
      finally syntheticVarCounter = savedCounter
    }

    // Record names

    def makeRecordFieldIdent(recIdent: Ident, fieldIdent: Ident)(
        implicit pos: Position): Ident =
      makeRecordFieldIdent(recIdent.name, recIdent.originalName,
          fieldIdent.name, fieldIdent.originalName)

    def makeRecordFieldIdent(recIdent: Ident,
        fieldName: String, fieldOrigiName: Option[String])(
        implicit pos: Position): Ident =
      makeRecordFieldIdent(recIdent.name, recIdent.originalName,
          fieldName, fieldOrigiName)

    def makeRecordFieldIdent(recName: String, recOrigName: Option[String],
        fieldName: String, fieldOrigName: Option[String])(
        implicit pos: Position): Ident = {
      val name = recName + "_$_" + fieldName
      val originalName = Some(recOrigName.getOrElse(recName) + "." +
          fieldOrigName.getOrElse(fieldName))
      Ident(name, originalName)
    }

    // LHS'es for labeled expressions

    var labeledExprLHSes: Map[Ident, Tree] = Map.empty

    // Now the work

    /** Desugar a statement of the IR into ES5 JS */
    def transformStat(tree: Tree)(implicit env: Env): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        // VarDefs at the end of block. Normal VarDefs are handled in
        // transformBlockStats

        case VarDef(_, _, _, EmptyTree) =>
          js.Skip()

        case VarDef(_, _, _, rhs) =>
          pushLhsInto(EmptyTree, rhs)

        // Statement-only language constructs

        case Skip() =>
          js.Skip()

        case Assign(RecordFieldVarRef(lhs), rhs) =>
          pushLhsInto(Assign(lhs, EmptyTree), rhs)

        case Assign(select @ Select(qualifier, item), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs, env0) =>
            implicit val env = env0
            js.Assign(
                js.DotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case Assign(select @ ArraySelect(array, index), rhs) =>
          unnest(List(array, index, rhs)) {
            case (List(newArray, newIndex, newRhs), env0) =>
              implicit val env = env0
              js.Assign(
                  js.BracketSelect(js.DotSelect(transformExpr(newArray),
                      js.Ident("u"))(select.pos),
                      transformExpr(newIndex))(select.pos),
                  transformExpr(newRhs))
          }

        case Assign(select @ JSDotSelect(qualifier, item), rhs) =>
          unnest(qualifier, rhs) { (newQualifier, newRhs, env0) =>
            implicit val env = env0
            js.Assign(
                js.DotSelect(transformExpr(newQualifier), item)(select.pos),
                transformExpr(newRhs))
          }

        case Assign(select @ JSBracketSelect(qualifier, item), rhs) =>
          unnest(List(qualifier, item, rhs)) {
            case (List(newQualifier, newItem, newRhs), env0) =>
              implicit val env = env0
              js.Assign(
                  js.BracketSelect(transformExpr(newQualifier),
                      transformExpr(newItem))(select.pos),
                  transformExpr(newRhs))
          }

        case Assign(_ : VarRef, rhs) =>
          pushLhsInto(tree, rhs)

        case Assign(_, _) =>
          sys.error(s"Illegal Assign in transformStat: $tree")

        case StoreModule(cls, value) =>
          unnest(value) { (newValue, env0) =>
            implicit val env = env0
            js.Assign(
                envField("n", cls.className),
                transformExpr(newValue))
          }

        case While(cond, body, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          val newLabel = label.map(transformIdent)
          if (isExpression(cond)) {
            js.While(transformExpr(cond), transformStat(body), newLabel)
          } else {
            js.While(js.BooleanLiteral(true), {
              unnest(cond) { (newCond, env0) =>
                implicit val env = env0
                js.If(transformExpr(newCond), transformStat(body), js.Break())
              }
            }, newLabel)
          }

        case DoWhile(body, cond, label) =>
          /* We cannot simply unnest(cond) here, because that would eject the
           * evaluation of the condition out of the loop.
           */
          val newLabel = label.map(transformIdent)
          if (isExpression(cond)) {
            js.DoWhile(transformStat(body), transformExpr(cond), newLabel)
          } else {
            /* This breaks 'continue' statements for this loop, but we don't
             * care because we never emit continue statements for do..while
             * loops.
             */
            js.While(js.BooleanLiteral(true), {
              js.Block(transformStat(body), {
                unnest(cond) { (newCond, env0) =>
                  implicit val env = env0
                  js.If(transformExpr(newCond), js.Skip(), js.Break())
                }
              })
            }, newLabel)
          }

        case Debugger() =>
          js.Debugger()

        case JSDelete(JSDotSelect(obj, prop)) =>
          unnest(obj) { (newObj, env0) =>
            implicit val env = env0
            js.Delete(js.DotSelect(transformExpr(newObj), prop))
          }

        case JSDelete(JSBracketSelect(obj, prop)) =>
          unnest(obj, prop) { (newObj, newProp, env0) =>
            implicit val env = env0
            js.Delete(js.BracketSelect(
                transformExpr(newObj), transformExpr(newProp)))
          }

        // Treat 'return' as an LHS

        case Return(expr, label) =>
          pushLhsInto(tree, expr)

        /* Anything else is an expression => pushLhsInto(EmptyTree, _)
         * In order not to duplicate all the code of pushLhsInto() here, we
         * use a trick: EmptyTree is a dummy LHS that says "do nothing
         * with the result of the rhs".
         * This is exactly what an expression statement is doing: it evaluates
         * the expression, but does nothing with its result.
         */

        case _ =>
          pushLhsInto(EmptyTree, tree)
      }
    }

    private object RecordFieldVarRef {
      def unapply(tree: Tree): Option[VarRef] = {
        tree match {
          case Select(RecordVarRef(VarRef(recIdent)), fieldIdent) =>
            implicit val pos = tree.pos
            Some(VarRef(makeRecordFieldIdent(recIdent, fieldIdent))(tree.tpe))
          case _ =>
            None
        }
      }
    }

    def transformBlockStats(trees: List[Tree])(
        implicit env: Env): (List[js.Tree], Env) = {

      @tailrec
      def transformLoop(trees: List[Tree], env: Env,
          acc: List[js.Tree]): (List[js.Tree], Env) = trees match {
        case (a @ LoadModule(m)) :: ts if env.initializedModules(m) =>
          transformLoop(ts, env, js.Skip()(a.pos) :: acc)

        case (a @ LoadModule(m)) :: ts =>
          transformLoop(ts, env.withInitialized(m), transformStat(a)(env) :: acc)

        case (tree @ VarDef(ident, tpe, mutable, rhs)) :: ts =>
          val newEnv = env.withDef(ident, tpe, mutable)
          val newTree = pushLhsInto(tree, rhs)(env)
          transformLoop(ts, newEnv, newTree :: acc)

        case tree :: ts =>
          transformLoop(ts, env, transformStat(tree)(env) :: acc)

        case Nil =>
          (acc.reverse, env)
      }

      transformLoop(trees, env, Nil)
    }

    private object RecordVarRef {
      def unapply(tree: Tree): Option[VarRef] = {
        if (!tree.tpe.isInstanceOf[RecordType]) None
        else {
          tree match {
            case tree: VarRef => Some(tree)
            case Select(RecordVarRef(VarRef(recIdent)), fieldIdent) =>
              implicit val pos = tree.pos
              Some(VarRef(makeRecordFieldIdent(recIdent, fieldIdent))(tree.tpe))
          }
        }
      }
    }

    /** Unnest complex constructs in argument position in temporary variables
     *
     *  If all the arguments are JS expressions, there is nothing to do.
     *  Any argument that is not a JS expression must be unnested and stored
     *  in a temporary variable before the statement produced by `makeStat`.
     *
     *  But *this changes the evaluation order!* In order not to lose it, it
     *  is necessary to also unnest arguments that are expressions but that
     *  are supposed to be evaluated before the argument-to-be-unnested and
     *  could have side-effects or even whose evaluation could be influenced
     *  by the side-effects of another unnested argument.
     *
     *  Without deep effect analysis, which we do not do, we need to take
     *  a very pessimistic approach, and unnest any expression that contains
     *  an identifier (except those after the last non-expression argument).
     *  Hence the predicate `isPureExpressionWithoutIdent`.
     */
    def unnest(args: List[Tree])(makeStat: (List[Tree], Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      if (args forall isExpression) makeStat(args, env)
      else {
        val extractedStatements = new scala.collection.mutable.ListBuffer[js.Tree]
        var innerEnv = env

        /* Attention! Everything must be processed recursively
         * *right-to-left*! Indeed, the point is that noExtractYet will tell
         * whether anything supposed to be evaluated *after* the currently
         * being processed expression has been (at least partly) extracted
         * in temporary variables (or simply statements, in the Block case).
         * If nothing has, we can keep more in place without having to extract
         * that expression in a temporary variable.
         *
         * Also note that environments are handled the wrong way around. This is
         * ok, since the same local name may not appear multiple times inside a
         * single method.
         */

        def rec(arg: Tree)(implicit env: Env): Tree = {
          def noExtractYet = extractedStatements.isEmpty

          if (if (noExtractYet) isExpression(arg) else isPureExpression(arg)) {
            arg
          } else {
            implicit val pos = arg.pos
            arg match {
              case Block(stats :+ expr) =>
                val (jsStats, newEnv) = transformBlockStats(stats)
                innerEnv = newEnv
                val result = rec(expr)(newEnv) // right-to-left, remember?
                // Put the stats in a Block because ++=: is not smart
                js.Block(jsStats) +=: extractedStatements
                result

              case UnaryOp(op, lhs) =>
                UnaryOp(op, rec(lhs))
              case BinaryOp(op, lhs, rhs) =>
                val newRhs = rec(rhs)
                BinaryOp(op, rec(lhs), newRhs)
              case JSBinaryOp(op, lhs, rhs) =>
                val newRhs = rec(rhs)
                JSBinaryOp(op, rec(lhs), newRhs)
              case JSUnaryOp(op, lhs) =>
                JSUnaryOp(op, rec(lhs))
              case IsInstanceOf(expr, tpe) =>
                IsInstanceOf(rec(expr), tpe)

              case AsInstanceOf(expr, tpe)
                  if noExtractYet || semantics.asInstanceOfs == Unchecked =>
                AsInstanceOf(rec(expr), tpe)
              case Unbox(expr, tpe)
                  if noExtractYet || semantics.asInstanceOfs == Unchecked =>
                Unbox(rec(expr), tpe)

              case NewArray(tpe, lengths) =>
                NewArray(tpe, recs(lengths))
              case ArrayValue(tpe, elems) =>
                ArrayValue(tpe, recs(elems))
              case JSArrayConstr(items) if !containsAnySpread(items) =>
                JSArrayConstr(recs(items))
              case JSObjectConstr(items) =>
                val newValues = recs(items.map(_._2))
                JSObjectConstr(items.map(_._1) zip newValues)
              case Closure(captureParams, params, body, captureValues) =>
                Closure(captureParams, params, body, recs(captureValues))

              case New(cls, constr, args) if noExtractYet =>
                New(cls, constr, recs(args))
              case Select(qualifier, item) if noExtractYet =>
                Select(rec(qualifier), item)(arg.tpe)
              case Apply(receiver, method, args) if noExtractYet =>
                val newArgs = recs(args)
                Apply(rec(receiver), method, newArgs)(arg.tpe)
              case ApplyStatically(receiver, cls, method, args) if noExtractYet =>
                val newArgs = recs(args)
                ApplyStatically(rec(receiver), cls, method, newArgs)(arg.tpe)
              case ApplyStatic(cls, method, args) if noExtractYet =>
                ApplyStatic(cls, method, recs(args))(arg.tpe)
              case ArrayLength(array) if noExtractYet =>
                ArrayLength(rec(array))
              case ArraySelect(array, index) if noExtractYet =>
                val newIndex = rec(index)
                ArraySelect(rec(array), newIndex)(arg.tpe)
              case CallHelper(helper, args) if noExtractYet =>
                CallHelper(helper, recs(args))(arg.tpe)

              case If(cond, thenp, elsep)
                  if noExtractYet && isExpression(thenp) && isExpression(elsep) =>
                If(rec(cond), thenp, elsep)(arg.tpe)

              case _ =>
                val temp = newSyntheticVar()
                val newEnv = env.withDef(temp, arg.tpe, false)
                innerEnv = newEnv
                val computeTemp = pushLhsInto(
                    VarDef(temp, arg.tpe, mutable = false, EmptyTree), arg)
                computeTemp +=: extractedStatements
                VarRef(temp)(arg.tpe)
            }
          }
        }

        def recs(args: List[Tree])(implicit env: Env): List[Tree] = {
          // This is a right-to-left map
          args.foldRight[List[Tree]](Nil) { (arg, acc) =>
            rec(arg) :: acc
          }
        }

        val newArgs = recs(args)

        assert(extractedStatements.nonEmpty,
            "Reached computeTemps with no temp to compute")

        val newStatement = makeStat(newArgs, innerEnv)
        js.Block(extractedStatements.result() ::: List(newStatement))(newStatement.pos)
      }
    }

    /** Same as above, for a single argument */
    def unnest(arg: Tree)(makeStat: (Tree, Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnest(List(arg)) {
        case (List(newArg), env) => makeStat(newArg, env)
      }
    }

    /** Same as above, for two arguments */
    def unnest(lhs: Tree, rhs: Tree)(
        makeStat: (Tree, Tree, Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnest(List(lhs, rhs)) {
        case (List(newLhs, newRhs), env) => makeStat(newLhs, newRhs, env)
      }
    }

    /** Same as above, for one head argument and a list of arguments */
    def unnest(arg0: Tree, args: List[Tree])(
        makeStat: (Tree, List[Tree], Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      unnest(arg0 :: args) { (newArgs, env) =>
        makeStat(newArgs.head, newArgs.tail, env)
      }
    }

    /** Common implementation for the functions below.
     *  A pure expression can be moved around or executed twice, because it
     *  will always produce the same result and never have side-effects.
     *  A side-effect free expression can be elided if its result is not used.
     */
    private def isExpressionInternal(tree: Tree, allowUnpure: Boolean,
        allowSideEffects: Boolean)(implicit env: Env): Boolean = {

      require(!allowSideEffects || allowUnpure)

      def test(tree: Tree): Boolean = tree match {
        // Atomic expressions
        case _: Literal   => true
        case _: This      => true
        case _: JSEnvInfo => true

        // Vars (side-effect free, pure if immutable)
        case VarRef(name) =>
          allowUnpure || !env.isLocalMutable(name)

        // Fields may throw if qualifier is null
        case Select(qualifier, item) =>
          allowSideEffects && test(qualifier)

        // Expressions preserving pureness
        case Block(trees)            => trees forall test
        case If(cond, thenp, elsep)  => test(cond) && test(thenp) && test(elsep)
        case BinaryOp(_, lhs, rhs)   => test(lhs) && test(rhs)
        case UnaryOp(_, lhs)         => test(lhs)
        case JSBinaryOp(_, lhs, rhs) => test(lhs) && test(rhs)
        case JSUnaryOp(_, lhs)       => test(lhs)
        case ArrayLength(array)      => test(array)
        case IsInstanceOf(expr, _)   => test(expr)

        // Expressions preserving side-effect freedom
        case NewArray(tpe, lengths) =>
          allowUnpure && (lengths forall test)
        case ArrayValue(tpe, elems) =>
          allowUnpure && (elems forall test)
        case ArraySelect(array, index) =>
          allowUnpure && test(array) && test(index)
        case JSArrayConstr(items) =>
          allowUnpure && (items forall test)
        case JSObjectConstr(items) =>
          allowUnpure && (items forall (item => test(item._2)))
        case Closure(captureParams, params, body, captureValues) =>
          allowUnpure && (captureValues forall test)

        // Scala expressions that can always have side-effects
        case New(cls, constr, args) =>
          allowSideEffects && (args forall test)
        case LoadModule(cls) => // unfortunately
          allowSideEffects
        case Apply(receiver, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case ApplyStatically(receiver, cls, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case ApplyStatic(cls, method, args) =>
          allowSideEffects && (args forall test)
        case GetClass(arg) =>
          allowSideEffects && test(arg)
        case CallHelper(helper, args) =>
          allowSideEffects && (args forall test)

        // Casts
        case AsInstanceOf(expr, _) =>
          (allowSideEffects || semantics.asInstanceOfs == Unchecked) && test(expr)
        case Unbox(expr, _) =>
          (allowSideEffects || semantics.asInstanceOfs == Unchecked) && test(expr)

        // Because the env is a frozen object, env["global"] is pure
        case JSBracketSelect(JSEnvInfo(), StringLiteral("global")) => true

        // JavaScript expressions that can always have side-effects
        case JSNew(fun, args) =>
          allowSideEffects && test(fun) && (args forall test)
        case JSDotSelect(qualifier, item) =>
          allowSideEffects && test(qualifier)
        case JSBracketSelect(qualifier, item) =>
          allowSideEffects && test(qualifier) && test(item)
        case JSFunctionApply(fun, args) =>
          allowSideEffects && test(fun) && (args forall test)
        case JSDotMethodApply(receiver, method, args) =>
          allowSideEffects && test(receiver) && (args forall test)
        case JSBracketMethodApply(receiver, method, args) =>
          allowSideEffects && test(receiver) && test(method) && (args forall test)

        // Non-expressions
        case _ => false
      }
      test(tree)
    }

    /** Test whether the given tree is a standard JS expression.
     */
    def isExpression(tree: Tree)(implicit env: Env): Boolean =
      isExpressionInternal(tree, allowUnpure = true, allowSideEffects = true)

    /** Test whether the given tree is a side-effect-free standard JS expression.
     */
    def isSideEffectFreeExpression(tree: Tree)(implicit env: Env): Boolean =
      isExpressionInternal(tree, allowUnpure = true, allowSideEffects = false)

    /** Test whether the given tree is a pure standard JS expression.
     */
    def isPureExpression(tree: Tree)(implicit env: Env): Boolean =
      isExpressionInternal(tree, allowUnpure = false, allowSideEffects = false)

    def doVarDef(ident: Ident, tpe: Type,
        rhs: Tree)(implicit env: Env): js.Tree = {
      implicit val pos = rhs.pos
      tpe match {
        case RecordType(fields) =>
          val elems = (rhs: @unchecked) match {
            case RecordValue(_, elems) =>
              elems
            case VarRef(rhsIdent) =>
              for (RecordType.Field(fName, fOrigName, fTpe, _) <- fields)
                yield VarRef(makeRecordFieldIdent(rhsIdent, fName, fOrigName))(fTpe)
          }
          js.Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doVarDef(makeRecordFieldIdent(ident, fName, fOrigName), fTpe, fRhs)
          })

        case _ =>
          js.VarDef(ident, transformExpr(rhs))
      }
    }

    def doEmptyVarDef(ident: Ident, tpe: Type)(
        implicit pos: Position, env: Env): js.Tree = {
      tpe match {
        case RecordType(fields) =>
          js.Block(for {
            RecordType.Field(fName, fOrigName, fTpe, fMutable) <- fields
          } yield {
            doEmptyVarDef(makeRecordFieldIdent(ident, fName, fOrigName), fTpe)
          })

        case _ =>
          js.VarDef(ident, js.EmptyTree)
      }
    }

    def doAssign(lhs: Tree, rhs: Tree)(implicit env: Env): js.Tree = {
      implicit val pos = rhs.pos
      lhs.tpe match {
        case RecordType(fields) =>
          val VarRef(ident) = lhs
          val elems = (rhs: @unchecked) match {
            case VarRef(rhsIdent) =>
              for (RecordType.Field(fName, fOrigName, fTpe, fMutable) <- fields)
                yield VarRef(makeRecordFieldIdent(rhsIdent, fName, fOrigName))(fTpe)
          }
          js.Block(for {
            (RecordType.Field(fName, fOrigName, fTpe, fMutable),
                fRhs) <- fields zip elems
          } yield {
            doAssign(VarRef(makeRecordFieldIdent(ident, fName, fOrigName))(fTpe), fRhs)
          })

        case _ =>
          js.Assign(transformExpr(lhs), transformExpr(rhs))
      }
    }

    /** Push an lhs into a (potentially complex) rhs
     *  lhs can be either a EmptyTree, a VarDef, a Assign or a
     *  Return
     */
    def pushLhsInto(lhs: Tree, rhs: Tree)(implicit env: Env): js.Tree = {
      implicit val rhsPos = rhs.pos

      /** Push the current lhs further into a deeper rhs. */
      @inline def redo(newRhs: Tree)(implicit env: Env) =
        pushLhsInto(lhs, newRhs)

      if (rhs.tpe == NothingType && lhs != EmptyTree) {
        /* A touch of peephole dead code elimination.
         * Actually necessary to handle pushing an lhs into an infinite loop,
         * for example.
         */
        val transformedRhs = pushLhsInto(EmptyTree, rhs)
        lhs match {
          case VarDef(name, tpe, _, _) =>
            /* We still need to declare the var, in case it is used somewhere
             * else in the function, where we can't dce it.
             */
            js.Block(doEmptyVarDef(name, tpe), transformedRhs)

          case _ =>
            transformedRhs
        }
      } else (rhs match {
        // Handle the Block before testing whether it is an expression

        case Block(stats0 :+ expr0) =>
          val (stats1, env0) = transformBlockStats(stats0)
          val expr1 = redo(expr0)(env0)
          js.Block(stats1 :+ expr1)

        // Base case, rhs is already a regular JS expression

        case _ if isExpression(rhs) =>
          (lhs: @unchecked) match {
            case EmptyTree =>
              if (isSideEffectFreeExpression(rhs)) js.Skip()
              else transformExpr(rhs)
            case VarDef(name, tpe, _, _) =>
              doVarDef(name, tpe, rhs)
            case Assign(lhs, _) =>
              doAssign(lhs, rhs)
            case Return(_, None) =>
              js.Return(transformExpr(rhs))
            case Return(_, label @ Some(l)) =>
              labeledExprLHSes(l) match {
                case newLhs @ Return(_, _) =>
                  pushLhsInto(newLhs, rhs) // no need to break here
                case newLhs =>
                  js.Block(pushLhsInto(newLhs, rhs),
                      js.Break(label.map(transformIdent)))
              }
          }

        // Almost base case with RecordValue

        case RecordValue(recTpe, elems) =>
          (lhs: @unchecked) match {
            case EmptyTree =>
              val (newStat, _) = transformBlockStats(elems)
              js.Block(newStat)
            case VarDef(name, tpe, _, _) =>
              unnest(elems) { (newElems, env) =>
                doVarDef(name, tpe, RecordValue(recTpe, newElems))(env)
              }
            case Assign(lhs, _) =>
              unnest(elems) { (newElems, env0) =>
                implicit val env = env0
                val temp = newSyntheticVar()
                js.Block(
                    doVarDef(temp, recTpe, RecordValue(recTpe, newElems)),
                    doAssign(lhs, VarRef(temp)(recTpe)))
              }
            case Return(_, label @ Some(l)) =>
              val newLhs = labeledExprLHSes(l)
              js.Block(pushLhsInto(newLhs, rhs),
                  js.Break(label.map(transformIdent)))
          }

        // Control flow constructs

        case Labeled(label, tpe, body) =>
          val savedMap = labeledExprLHSes
          labeledExprLHSes = labeledExprLHSes + (label -> lhs)
          try {
            lhs match {
              case Return(_, _) => redo(body)
              case _            => js.Labeled(label, redo(body))
            }
          } finally {
            labeledExprLHSes = savedMap
          }

        case Return(expr, _) =>
          pushLhsInto(rhs, expr)

        case Continue(label) =>
          js.Continue(label.map(transformIdent))

        case If(cond, thenp, elsep) =>
          unnest(cond) { (newCond, env0) =>
            implicit val env = env0
            js.If(transformExpr(newCond), redo(thenp), redo(elsep))
          }

        case Try(block, errVar, handler, finalizer) =>
          val newHandler =
            if (handler == EmptyTree) js.EmptyTree else redo(handler)
          val newFinalizer =
            if (finalizer == EmptyTree) js.EmptyTree else transformStat(finalizer)

          if (newHandler != js.EmptyTree && newFinalizer != js.EmptyTree) {
            /* The Google Closure Compiler wrongly eliminates finally blocks, if
             * the catch block throws an exception.
             * Issues: #563, google/closure-compiler#186
             *
             * Therefore, we desugar
             *
             *   try { ... } catch { ... } finally { ... }
             *
             * into
             *
             *   try { try { ... } catch { ... } } finally { ... }
             */
            js.Try(js.Try(redo(block), errVar, newHandler, js.EmptyTree),
                errVar, js.EmptyTree, newFinalizer)
          } else
            js.Try(redo(block), errVar, newHandler, newFinalizer)

        // TODO Treat throw as an LHS?
        case Throw(expr) =>
          unnest(expr) { (newExpr, env) =>
            js.Throw(transformExpr(newExpr)(env))
          }

        /** Matches are desugared into switches
         *
         *  A match is different from a switch in two respects, both linked
         *  to match being designed to be used in expression position in
         *  Extended-JS.
         *
         *  * There is no fall-through from one case to the next one, hence,
         *    no break statement.
         *  * Match supports _alternatives_ explicitly (with a switch, one
         *    would use the fall-through behavior to implement alternatives).
         */
        case Match(selector, cases, default) =>
          unnest(selector) { (newSelector, env0) =>
            implicit val env = env0
            val newCases = {
              for {
                (values, body) <- cases
                newValues = (values map transformExpr)
                // add the break statement
                newBody = js.Block(redo(body), js.Break())
                // desugar alternatives into several cases falling through
                caze <- (newValues.init map (v => (v, js.Skip()))) :+ (newValues.last, newBody)
              } yield {
                caze
              }
            }
            val newDefault =
              if (default == EmptyTree) js.EmptyTree
              else redo(default)
            js.Switch(transformExpr(newSelector), newCases, newDefault)
          }

        // Scala expressions (if we reach here their arguments are not expressions)

        case New(cls, ctor, args) =>
          unnest(args) { (newArgs, env) =>
            redo(New(cls, ctor, newArgs))(env)
          }

        case Select(qualifier, item) =>
          unnest(qualifier) { (newQualifier, env) =>
            redo(Select(newQualifier, item)(rhs.tpe))(env)
          }

        case Apply(receiver, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs, env) =>
            redo(Apply(newReceiver, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyStatically(receiver, cls, method, args) =>
          unnest(receiver, args) { (newReceiver, newArgs, env) =>
            redo(ApplyStatically(newReceiver, cls, method, newArgs)(rhs.tpe))(env)
          }

        case ApplyStatic(cls, method, args) =>
          unnest(args) { (newArgs, env) =>
            redo(ApplyStatic(cls, method, newArgs)(rhs.tpe))(env)
          }

        case UnaryOp(op, lhs) =>
          unnest(lhs) { (newLhs, env) =>
            redo(UnaryOp(op, newLhs))(env)
          }

        case BinaryOp(op, lhs, rhs) =>
          unnest(lhs, rhs) { (newLhs, newRhs, env) =>
            redo(BinaryOp(op, newLhs, newRhs))(env)
          }

        case NewArray(tpe, lengths) =>
          unnest(lengths) { (newLengths, env) =>
            redo(NewArray(tpe, newLengths))(env)
          }

        case ArrayValue(tpe, elems) =>
          unnest(elems) { (newElems, env) =>
            redo(ArrayValue(tpe, newElems))(env)
          }

        case ArrayLength(array) =>
          unnest(array) { (newArray, env) =>
            redo(ArrayLength(newArray))(env)
          }

        case ArraySelect(array, index) =>
          unnest(array, index) { (newArray, newIndex, env) =>
            redo(ArraySelect(newArray, newIndex)(rhs.tpe))(env)
          }

        case IsInstanceOf(expr, cls) =>
          unnest(expr) { (newExpr, env) =>
            redo(IsInstanceOf(newExpr, cls))(env)
          }

        case AsInstanceOf(expr, cls) =>
          if (semantics.asInstanceOfs == Unchecked) {
            redo(expr)
          } else {
            unnest(expr) { (newExpr, env) =>
              redo(AsInstanceOf(newExpr, cls))(env)
            }
          }

        case Unbox(expr, charCode) =>
          unnest(expr) { (newExpr, env) =>
            redo(Unbox(newExpr, charCode))(env)
          }

        case GetClass(expr) =>
          unnest(expr) { (newExpr, env) =>
            redo(GetClass(newExpr))(env)
          }

        case CallHelper(helper, args) =>
          unnest(args) { (newArgs, env) =>
            redo(CallHelper(helper, newArgs)(rhs.tpe))(env)
          }

        // JavaScript expressions (if we reach here their arguments are not expressions)

        case JSNew(ctor, args) =>
          if (containsAnySpread(args)) {
            redo {
              CallHelper("newJSObjectWithVarargs",
                  List(ctor, spreadToArgArray(args)))(AnyType)
            }
          } else {
            unnest(ctor :: args) { (newCtorAndArgs, env) =>
              val newCtor :: newArgs = newCtorAndArgs
              redo(JSNew(newCtor, newArgs))(env)
            }
          }

        case JSFunctionApply(fun, args) =>
          if (containsAnySpread(args)) {
            redo {
              JSBracketMethodApply(fun, StringLiteral("apply"),
                  List(Undefined(), spreadToArgArray(args)))
            }
          } else {
            unnest(fun :: args) { (newFunAndArgs, env) =>
              val newFun :: newArgs = newFunAndArgs
              redo(JSFunctionApply(newFun, newArgs))(env)
            }
          }

        case JSDotMethodApply(receiver, method, args) =>
          if (containsAnySpread(args)) {
            withTempVar(receiver) { (newReceiver, env0) =>
              implicit val env = env0
              redo {
                JSBracketMethodApply(
                    JSDotSelect(newReceiver, method),
                    StringLiteral("apply"),
                    List(newReceiver, spreadToArgArray(args)))
              }
            }
          } else {
            unnest(receiver :: args) { (newReceiverAndArgs, env) =>
              val newReceiver :: newArgs = newReceiverAndArgs
              redo(JSDotMethodApply(newReceiver, method, newArgs))(env)
            }
          }

        case JSBracketMethodApply(receiver, method, args) =>
          if (containsAnySpread(args)) {
            withTempVar(receiver) { (newReceiver, env0) =>
              implicit val env = env0
              redo {
                JSBracketMethodApply(
                    JSBracketSelect(newReceiver, method),
                    StringLiteral("apply"),
                    List(newReceiver, spreadToArgArray(args)))
              }
            }
          } else {
            unnest(receiver :: method :: args) { (newReceiverAndArgs, env) =>
              val newReceiver :: newMethod :: newArgs = newReceiverAndArgs
              redo(JSBracketMethodApply(newReceiver, newMethod, newArgs))(env)
            }
          }

        case JSDotSelect(qualifier, item) =>
          unnest(qualifier) { (newQualifier, env) =>
            redo(JSDotSelect(newQualifier, item))(env)
          }

        case JSBracketSelect(qualifier, item) =>
          unnest(qualifier, item) { (newQualifier, newItem, env) =>
            redo(JSBracketSelect(newQualifier, newItem))(env)
          }

        case JSUnaryOp(op, lhs) =>
          unnest(lhs) { (newLhs, env) =>
            redo(JSUnaryOp(op, newLhs))(env)
          }

        case JSBinaryOp(JSBinaryOp.&&, lhs, rhs) =>
          if (lhs.tpe == BooleanType) {
            redo(If(lhs, rhs, BooleanLiteral(false))(AnyType))
          } else {
            unnest(lhs) { (newLhs, env) =>
              redo(If(newLhs, rhs, newLhs)(AnyType))(env)
            }
          }

        case JSBinaryOp(JSBinaryOp.||, lhs, rhs) =>
          if (lhs.tpe == BooleanType) {
            redo(If(lhs, BooleanLiteral(true), rhs)(AnyType))
          } else {
            unnest(lhs) { (newLhs, env) =>
              redo(If(newLhs, newLhs, rhs)(AnyType))(env)
            }
          }

        case JSBinaryOp(op, lhs, rhs) =>
          unnest(lhs, rhs) { (newLhs, newRhs, env) =>
            redo(JSBinaryOp(op, newLhs, newRhs))(env)
          }

        case JSArrayConstr(items) =>
          if (containsAnySpread(items)) {
            redo {
              spreadToArgArray(items)
            }
          } else {
            unnest(items) { (newItems, env) =>
              redo(JSArrayConstr(newItems))(env)
            }
          }

        case JSObjectConstr(fields) =>
          val names = fields map (_._1)
          val items = fields map (_._2)
          unnest(items) { (newItems, env) =>
            redo(JSObjectConstr(names.zip(newItems)))(env)
          }

        // Closures

        case Closure(captureParams, params, body, captureValues) =>
          unnest(captureValues) { (newCaptureValues, env) =>
            redo(Closure(captureParams, params, body, newCaptureValues))(env)
          }

        case _ =>
          if (lhs == EmptyTree) {
            /* Go "back" to transformStat() after having dived into
             * expression statements. Remember that (lhs == EmptyTree)
             * is a trick that we use to "add" all the code of pushLhsInto()
             * to transformStat().
             */
            rhs match {
              case _:Skip | _:VarDef | _:Assign | _:While | _:DoWhile |
                  _:Debugger | _:JSDelete | _:StoreModule | _:ClassDef =>
                transformStat(rhs)
              case _ =>
                sys.error("Illegal tree in JSDesugar.pushLhsInto():\n" +
                    "lhs = " + lhs + "\n" + "rhs = " + rhs +
                    " of class " + rhs.getClass)
            }
          } else {
            sys.error("Illegal tree in JSDesugar.pushLhsInto():\n" +
                "lhs = " + lhs + "\n" + "rhs = " + rhs +
                " of class " + rhs.getClass)
          }
      })
    }

    private def containsAnySpread(args: List[Tree]): Boolean =
      args.exists(_.isInstanceOf[JSSpread])

    private def spreadToArgArray(args: List[Tree])(
        implicit env: Env, pos: Position): Tree = {
      var reversedParts: List[Tree] = Nil
      var reversedPartUnderConstruction: List[Tree] = Nil

      def closeReversedPartUnderConstruction() = {
        if (!reversedPartUnderConstruction.isEmpty) {
          val part = reversedPartUnderConstruction.reverse
          reversedParts ::= JSArrayConstr(part)(part.head.pos)
          reversedPartUnderConstruction = Nil
        }
      }

      for (arg <- args) {
        arg match {
          case JSSpread(spreadArray) =>
            closeReversedPartUnderConstruction()
            reversedParts ::= spreadArray
          case _ =>
            reversedPartUnderConstruction ::= arg
        }
      }
      closeReversedPartUnderConstruction()

      reversedParts match {
        case Nil        => JSArrayConstr(Nil)
        case List(part) => part
        case _          =>
          val partHead :: partTail = reversedParts.reverse
          JSBracketMethodApply(
              partHead, StringLiteral("concat"), partTail)
      }
    }

    /** Evaluates `expr` and stores the result in a temp, then evaluates the
     *  result of `makeTree(temp)`.
     */
    private def withTempVar(expr: Tree)(makeTree: (Tree, Env) => js.Tree)(
        implicit env: Env): js.Tree = {
      expr match {
        case VarRef(ident) if !env.isLocalMutable(ident) =>
          makeTree(expr, env)
        case _ =>
          implicit val pos = expr.pos
          val temp = newSyntheticVar()
          val newEnv = env.withDef(temp, expr.tpe, false)
          val computeTemp = pushLhsInto(
              VarDef(temp, expr.tpe, mutable = false, EmptyTree), expr)
          js.Block(computeTemp, makeTree(VarRef(temp)(expr.tpe), newEnv))
      }
    }

    // Desugar Scala operations to JavaScript operations -----------------------

    /** Desugar an expression of the IR into ES5 JS */
    def transformExpr(tree: Tree)(implicit env: Env): js.Tree = {
      import TreeDSL._

      implicit val pos = tree.pos

      def or0(tree: js.Tree): js.Tree =
        js.BinaryOp(JSBinaryOp.|, tree, js.IntLiteral(0))

      tree match {
        // Control flow constructs

        case Block(stats :+ expr) =>
          val (newStats, newEnv) = transformBlockStats(stats)
          js.Block(newStats :+ transformExpr(expr)(newEnv))

        // Note that these work even if thenp/elsep is not a BooleanType
        case If(cond, BooleanLiteral(true), elsep) =>
          js.BinaryOp(JSBinaryOp.||, transformExpr(cond), transformExpr(elsep))
        case If(cond, thenp, BooleanLiteral(false)) =>
          js.BinaryOp(JSBinaryOp.&&, transformExpr(cond), transformExpr(thenp))

        case If(cond, thenp, elsep) =>
          js.If(transformExpr(cond), transformExpr(thenp), transformExpr(elsep))

        // Scala expressions

        case New(cls, ctor, args) =>
          js.Apply(js.New(encodeClassVar(cls.className), Nil) DOT ctor,
              args map transformExpr)

        case LoadModule(cls) =>
          genLoadModule(cls.className, env.initializedModules(cls))

        case RecordFieldVarRef(VarRef(name)) =>
          js.VarRef(name)

        case Select(qualifier, item) =>
          transformExpr(qualifier) DOT item

        case Apply(a @ LoadModule(m), method, args) if !env.initializedModules(m) =>
          //TODO: does this need attention to hijacking?
          val newReceiver = transformExpr(a)
          val newEnv = env.withInitialized(m)
          val newArgs = args.map(transformExpr(_)(newEnv))
          if (isMaybeHijackedClass(a.tpe) &&
              !Definitions.isReflProxyName(method.name)) {
            val helperName = hijackedClassMethodToHelperName(method.name)
            genCallHelper(helperName, newReceiver :: newArgs: _*)
          } else {
            js.Apply(newReceiver DOT method, newArgs)
          }

        //TODO: abstract contents here with above
        case Apply(receiver, method, args) =>
          val newReceiver = transformExpr(receiver)
          val newArgs = args map transformExpr
          if (isMaybeHijackedClass(receiver.tpe) &&
              !Definitions.isReflProxyName(method.name)) {
            val helperName = hijackedClassMethodToHelperName(method.name)
            genCallHelper(helperName, newReceiver :: newArgs: _*)
          } else {
            js.Apply(newReceiver DOT method, newArgs)
          }

        case ApplyStatically(receiver, cls, method, args) =>
          val fun = encodeClassVar(cls.className).prototype DOT method
          js.Apply(fun DOT "call", (receiver :: args) map transformExpr)

        case ApplyStatic(cls, method, args) =>
          val Ident(methodName, origName) = method
          val fullName = cls.className + "__" + methodName
          js.Apply(envField("s", fullName, origName), args map transformExpr)

        case UnaryOp(op, lhs) =>
          import UnaryOp._
          val newLhs = transformExpr(lhs)
          (op: @switch) match {
            case Boolean_!   => js.UnaryOp(JSUnaryOp.!, newLhs)
            case DoubleToInt => js.BinaryOp(JSBinaryOp.|, newLhs, js.IntLiteral(0))

            case LongToInt    => genLongMethodApply(newLhs, LongImpl.toInt)
            case LongToDouble => genLongMethodApply(newLhs, LongImpl.toDouble)

            case DoubleToFloat => genFround(newLhs)

            case IntToLong =>
              genNewLong(LongImpl.initFromInt, newLhs)
            case DoubleToLong =>
              genLongModuleApply(LongImpl.fromDouble, newLhs)
          }

        case BinaryOp(op, lhs, rhs) =>
          import BinaryOp._
          val lhs1 = lhs match {
            case UnaryOp(UnaryOp.DoubleToInt, inner)
                if op == Int_& || op == Int_<< =>
              /* This case is emitted typically by conversions from
               * Float/Double to Char/Byte/Short. We have to introduce an
               * (int) cast in the IR so that it typechecks, but in JavaScript
               * this is redundant because & and << already convert both their
               * operands to ints. So we get rid of the conversion here.
               */
              inner
            case _ =>
              lhs
          }

          val newLhs = transformExpr(lhs1)
          val newRhs = transformExpr(rhs)

          (op: @switch) match {
            case === | Num_== | Boolean_== =>
              js.BinaryOp(JSBinaryOp.===, newLhs, newRhs)
            case !== | Num_!= | Boolean_!= =>
              js.BinaryOp(JSBinaryOp.!==, newLhs, newRhs)

            case String_+ =>
              if (lhs.tpe == StringType || rhs.tpe == StringType)
                js.BinaryOp(JSBinaryOp.+, newLhs, newRhs)
              else
                js.BinaryOp(JSBinaryOp.+, js.BinaryOp(JSBinaryOp.+,
                    js.StringLiteral(""), newLhs), newRhs)

            case Int_+ => or0(js.BinaryOp(JSBinaryOp.+, newLhs, newRhs))
            case Int_- =>
              lhs match {
                case IntLiteral(0) => or0(js.UnaryOp(JSUnaryOp.-, newRhs))
                case _             => or0(js.BinaryOp(JSBinaryOp.-, newLhs, newRhs))
              }
            case Int_* => genCallHelper("imul", newLhs, newRhs)
            case Int_/ => or0(js.BinaryOp(JSBinaryOp./, newLhs, newRhs))
            case Int_% => js.BinaryOp(JSBinaryOp.%, newLhs, newRhs)

            case Int_|   => js.BinaryOp(JSBinaryOp.|, newLhs, newRhs)
            case Int_&   => js.BinaryOp(JSBinaryOp.&, newLhs, newRhs)
            case Int_^   =>
              lhs match {
                case IntLiteral(-1) => js.UnaryOp(JSUnaryOp.~, newRhs)
                case _              => js.BinaryOp(JSBinaryOp.^, newLhs, newRhs)
              }
            case Int_<<  => js.BinaryOp(JSBinaryOp.<<, newLhs, newRhs)
            case Int_>>> => or0(js.BinaryOp(JSBinaryOp.>>>, newLhs, newRhs))
            case Int_>>  => js.BinaryOp(JSBinaryOp.>>, newLhs, newRhs)

            case Float_+ => genFround(js.BinaryOp(JSBinaryOp.+, newLhs, newRhs))
            case Float_- =>
              genFround(lhs match {
                case DoubleLiteral(0.0) => js.UnaryOp(JSUnaryOp.-, newRhs)
                case _                  => js.BinaryOp(JSBinaryOp.-, newLhs, newRhs)
              })
            case Float_* => genFround(js.BinaryOp(JSBinaryOp.*, newLhs, newRhs))
            case Float_/ => genFround(js.BinaryOp(JSBinaryOp./, newLhs, newRhs))
            case Float_% => genFround(js.BinaryOp(JSBinaryOp.%, newLhs, newRhs))

            case Double_+ => js.BinaryOp(JSBinaryOp.+, newLhs, newRhs)
            case Double_- =>
              lhs match {
                case DoubleLiteral(0.0) => js.UnaryOp(JSUnaryOp.-, newRhs)
                case _                  => js.BinaryOp(JSBinaryOp.-, newLhs, newRhs)
              }
            case Double_* => js.BinaryOp(JSBinaryOp.*, newLhs, newRhs)
            case Double_/ => js.BinaryOp(JSBinaryOp./, newLhs, newRhs)
            case Double_% => js.BinaryOp(JSBinaryOp.%, newLhs, newRhs)

            case Num_<  => js.BinaryOp(JSBinaryOp.< , newLhs, newRhs)
            case Num_<= => js.BinaryOp(JSBinaryOp.<=, newLhs, newRhs)
            case Num_>  => js.BinaryOp(JSBinaryOp.> , newLhs, newRhs)
            case Num_>= => js.BinaryOp(JSBinaryOp.>=, newLhs, newRhs)

            case Long_+ => genLongMethodApply(newLhs, LongImpl.+, newRhs)
            case Long_- =>
              lhs match {
                case LongLiteral(0L) => genLongMethodApply(newRhs, LongImpl.UNARY_-)
                case _               => genLongMethodApply(newLhs, LongImpl.-, newRhs)
              }
            case Long_* => genLongMethodApply(newLhs, LongImpl.*, newRhs)
            case Long_/ => genLongMethodApply(newLhs, LongImpl./, newRhs)
            case Long_% => genLongMethodApply(newLhs, LongImpl.%, newRhs)

            case Long_|   => genLongMethodApply(newLhs, LongImpl.|,   newRhs)
            case Long_&   => genLongMethodApply(newLhs, LongImpl.&,   newRhs)
            case Long_^   =>
              lhs match {
                case LongLiteral(-1L) => genLongMethodApply(newRhs, LongImpl.UNARY_~)
                case _                => genLongMethodApply(newLhs, LongImpl.^, newRhs)
              }
            case Long_<<  => genLongMethodApply(newLhs, LongImpl.<<,  newRhs)
            case Long_>>> => genLongMethodApply(newLhs, LongImpl.>>>, newRhs)
            case Long_>>  => genLongMethodApply(newLhs, LongImpl.>>,  newRhs)

            case Long_== => genLongMethodApply(newLhs, LongImpl.===, newRhs)
            case Long_!= => genLongMethodApply(newLhs, LongImpl.!==, newRhs)
            case Long_<  => genLongMethodApply(newLhs, LongImpl.<,   newRhs)
            case Long_<= => genLongMethodApply(newLhs, LongImpl.<=,  newRhs)
            case Long_>  => genLongMethodApply(newLhs, LongImpl.>,   newRhs)
            case Long_>= => genLongMethodApply(newLhs, LongImpl.>=,  newRhs)

            case Boolean_| => !(!js.BinaryOp(JSBinaryOp.|, newLhs, newRhs))
            case Boolean_& => !(!js.BinaryOp(JSBinaryOp.&, newLhs, newRhs))
          }

        case NewArray(tpe, lengths) =>
          genCallHelper("newArrayObject",
              genClassDataOf(tpe), js.ArrayConstr(lengths map transformExpr))

        case ArrayValue(tpe, elems) =>
          genCallHelper("makeNativeArrayWrapper",
              genClassDataOf(tpe), js.ArrayConstr(elems map transformExpr))

        case ArrayLength(array) =>
          js.BracketSelect(js.DotSelect(transformExpr(array),
              Ident("u")), js.StringLiteral("length"))

        case ArraySelect(array, index) =>
          js.BracketSelect(js.DotSelect(transformExpr(array),
              Ident("u")), transformExpr(index))

        case IsInstanceOf(expr, cls) =>
          genIsInstanceOf(transformExpr(expr), cls)

        case AsInstanceOf(expr, cls) =>
          val newExpr = transformExpr(expr)
          if (semantics.asInstanceOfs == Unchecked) newExpr
          else genAsInstanceOf(newExpr, cls)

        case Unbox(expr, charCode) =>
          val newExpr = transformExpr(expr)

          if (semantics.asInstanceOfs == Unchecked) {
            (charCode: @switch) match {
              case 'Z'             => !(!newExpr)
              case 'B' | 'S' | 'I' =>
                js.BinaryOp(JSBinaryOp.|, newExpr, js.IntLiteral(0))
              case 'J'             => genCallHelper("uJ", newExpr)
              case 'F'             => genFround(newExpr)
              case 'D'             => js.UnaryOp(JSUnaryOp.+, newExpr)
            }
          } else {
            genCallHelper("u"+charCode, newExpr)
          }

        case GetClass(expr) =>
          genCallHelper("objectGetClass", transformExpr(expr))

        case CallHelper(helper, args) =>
          genCallHelper(helper, args map transformExpr: _*)

        // JavaScript expressions

        case JSBracketSelect(JSEnvInfo(), StringLiteral("global")) =>
          // Shortcut for this field which is heavily used
          envField("g")

        case JSNew(constr, args) =>
          js.New(transformExpr(constr), args map transformExpr)

        case JSDotSelect(qualifier, item) =>
          js.DotSelect(transformExpr(qualifier), item)

        case JSBracketSelect(qualifier, item) =>
          js.BracketSelect(transformExpr(qualifier), transformExpr(item))

        case JSFunctionApply(fun, args) =>
          /* Protect the fun so that if it is, e.g.,
           * path.f
           * we emit
           * (0, path.f)(args...)
           * instead of
           * path.f(args...)
           * If we emit the latter, then `this` will be bound to `path` in
           * `f`, which is sometimes extremely harmful (e.g., for builtin
           * methods of `window`).
           */
          val transformedFun = transformExpr(fun)
          val protectedFun = transformedFun match {
            case _:js.DotSelect | _:js.BracketSelect =>
              js.Block(js.IntLiteral(0), transformedFun)
            case _ =>
              transformedFun
          }
          js.Apply(protectedFun, args map transformExpr)

        case JSDotMethodApply(receiver, method, args) =>
          js.Apply(js.DotSelect(transformExpr(receiver), method),
              args map transformExpr)

        case JSBracketMethodApply(receiver, method, args) =>
          js.Apply(js.BracketSelect(transformExpr(receiver),
              transformExpr(method)), args map transformExpr)

        case JSUnaryOp(op, lhs) =>
          js.UnaryOp(op, transformExpr(lhs))

        case JSBinaryOp(op, lhs, rhs) =>
          js.BinaryOp(op, transformExpr(lhs), transformExpr(rhs))

        case JSArrayConstr(items) =>
          js.ArrayConstr(items map transformExpr)

        case JSObjectConstr(fields) =>
          js.ObjectConstr(fields map {
            case (name: Ident, value) =>
              (transformIdent(name), transformExpr(value))
            case (StringLiteral(name), value) =>
              (js.StringLiteral(name), transformExpr(value))
          })

        case JSEnvInfo() =>
          envField("env")

        // Literals

        case Undefined()            => js.Undefined()
        case Null()                 => js.Null()
        case BooleanLiteral(value)  => js.BooleanLiteral(value)
        case IntLiteral(value)      => js.IntLiteral(value)
        case FloatLiteral(value)    => js.DoubleLiteral(value.toDouble)
        case DoubleLiteral(value)   => js.DoubleLiteral(value)
        case StringLiteral(value)   => js.StringLiteral(value)

        case LongLiteral(0L) =>
          genLongModuleApply(LongImpl.Zero)
        case LongLiteral(value) =>
          val (l, m, h) = LongImpl.extractParts(value)
          genNewLong(LongImpl.initFromParts,
              js.IntLiteral(l), js.IntLiteral(m), js.IntLiteral(h))

        case ClassOf(cls) =>
          js.Apply(js.DotSelect(genClassDataOf(cls), Ident("getClassOf")), Nil)

        // Atomic expressions

        case VarRef(name) =>
          js.VarRef(name)

        case This() =>
          js.This()

        case Closure(captureParams, params, body, captureValues) =>
          val transformedBody = {
            val env = Env.empty.withParams(captureParams ++ params)
            val withReturn = Return(body, None)
            (transformStat(withReturn)(env)) match {
              case js.Block(stats :+ js.Return(js.Undefined())) => js.Block(stats)
              case other                                        => other
            }
          }

          val innerFunction =
            js.Function(params.map(transformParamDef), transformedBody)

          if (captureParams.isEmpty) {
            innerFunction
          } else {
            js.Apply(
                js.Function(captureParams.map(transformParamDef), {
                  js.Return(innerFunction)
                }),
                captureValues.map(transformExpr))
          }

        // Invalid trees

        case _ =>
          sys.error("Invalid tree in JSDesugar.transformExpr() "+
              s"of class ${tree.getClass}")
      }
    }

    def isMaybeHijackedClass(tpe: Type): Boolean = tpe match {
      case ClassType(cls) =>
        Definitions.HijackedClasses.contains(cls) ||
        Definitions.AncestorsOfHijackedClasses.contains(cls)
      case AnyType | UndefType | BooleanType | IntType | LongType |
          FloatType | DoubleType | StringType =>
        true
      case _ =>
        false
    }

    val hijackedClassMethodToHelperName: Map[String, String] = Map(
        "toString__T"        -> "objectToString",
        "clone__O"           -> "objectClone",
        "finalize__V"        -> "objectFinalize",
        "notify__V"          -> "objectNotify",
        "notifyAll__V"       -> "objectNotifyAll",
        "equals__O__Z"       -> "objectEquals",
        "hashCode__I"        -> "objectHashCode",

        "length__I"                          -> "charSequenceLength",
        "charAt__I__C"                       -> "charSequenceCharAt",
        "subSequence__I__I__jl_CharSequence" -> "charSequenceSubSequence",

        "compareTo__O__I"          -> "comparableCompareTo",
        "compareTo__jl_Boolean__I" -> "comparableCompareTo",
        "compareTo__jl_Byte__I"    -> "comparableCompareTo",
        "compareTo__jl_Short__I"   -> "comparableCompareTo",
        "compareTo__jl_Integer__I" -> "comparableCompareTo",
        "compareTo__jl_Long__I"    -> "comparableCompareTo",
        "compareTo__jl_Float__I"   -> "comparableCompareTo",
        "compareTo__jl_Double__I"  -> "comparableCompareTo",
        "compareTo__jl_String__I"  -> "comparableCompareTo",

        "booleanValue__Z" -> "booleanBooleanValue",

        "byteValue__B"   -> "numberByteValue",
        "shortValue__S"  -> "numberShortValue",
        "intValue__I"    -> "numberIntValue",
        "longValue__J"   -> "numberLongValue",
        "floatValue__F"  -> "numberFloatValue",
        "doubleValue__D" -> "numberDoubleValue",

        "isNaN__Z"      -> "isNaN",
        "isInfinite__Z" -> "isInfinite"
    )

    def genClassDataOf(cls: ReferenceType)(implicit pos: Position): js.Tree = {
      cls match {
        case ClassType(className) =>
          envField("d", className)
        case ArrayType(base, dims) =>
          (1 to dims).foldLeft(envField("d", base)) { (prev, _) =>
            js.Apply(js.DotSelect(prev, js.Ident("getArrayOf")), Nil)
          }
      }
    }

    private def genFround(arg: js.Tree)(implicit pos: Position): js.Tree = {
      genCallHelper("fround", arg)
    }

    private def genNewLong(ctor: String, args: js.Tree*)(
        implicit pos: Position): js.Tree = {
      import TreeDSL._
      js.Apply(
          js.New(encodeClassVar(LongImpl.RuntimeLongClass), Nil) DOT ctor,
          args.toList)
    }

    private def genLongMethodApply(receiver: js.Tree, methodName: String,
        args: js.Tree*)(implicit pos: Position): js.Tree = {
      import TreeDSL._
      js.Apply(receiver DOT methodName, args.toList)
    }

    private def genLongModuleApply(methodName: String, args: js.Tree*)(
        implicit pos: Position): js.Tree = {
      import TreeDSL._
      js.Apply(
          genLoadModule(LongImpl.RuntimeLongModuleClass) DOT methodName,
          args.toList)
    }

    private def genLoadModule(moduleClass: String, hasInitialized: Boolean = false)(
        implicit pos: Position): js.Tree = {
      import TreeDSL._

      if(hasInitialized) envField("n", moduleClass)
      else js.Apply(envField("m", moduleClass), Nil)
    }

    private implicit class RecordAwareEnv(env: Env) {
      def withDef(ident: Ident, tpe: Type, mutable: Boolean): Env = tpe match {
        case RecordType(fields) =>
          withRecordDefs(ident, fields, mutable)
        case _ =>
          env.withDef(ident, mutable)
      }

      private def withRecordDefs(recIdent: Ident,
          fields: List[RecordType.Field], recMutable: Boolean): Env = {
        fields.foldLeft(env) { (env, fld) =>
          val ident = makeRecordFieldIdent(recIdent, fld.name,
              fld.originalName)(recIdent.pos)
          env.withDef(ident, fld.tpe, recMutable || fld.mutable)
        }
      }
    }
  }

  // Environment

  final class Env private (vars: Map[String, Boolean], val initializedModules: Set[ClassType]) {
    def isLocalMutable(ident: Ident): Boolean = vars(ident.name)

    def withParams(params: List[ParamDef]): Env = {
      params.foldLeft(this) {
        case (env, ParamDef(name, tpe, mutable)) =>
          // ParamDefs may not contain record types
          env.withDef(name, mutable)
      }
    }

    def withDef(ident: Ident, mutable: Boolean): Env =
      new Env(vars + (ident.name -> mutable), initializedModules)

    def withInitialized(module: ClassType) =
      new Env(vars, initializedModules + module)
  }

  object Env {
    def empty: Env = new Env(Map.empty, Set.empty)
  }

  // Helpers

  private[javascript] def genIsInstanceOf(expr: js.Tree, cls: ReferenceType)(
      implicit outputMode: OutputMode, pos: Position): js.Tree =
    genIsAsInstanceOf(expr, cls, test = true)

  private def genAsInstanceOf(expr: js.Tree, cls: ReferenceType)(
      implicit outputMode: OutputMode, pos: Position): js.Tree =
    genIsAsInstanceOf(expr, cls, test = false)

  private def genIsAsInstanceOf(expr: js.Tree, cls: ReferenceType, test: Boolean)(
      implicit outputMode: OutputMode, pos: Position): js.Tree = {
    import Definitions._
    import TreeDSL._

    cls match {
      case ClassType(className0) =>
        val className =
          if (className0 == BoxedLongClass) LongImpl.RuntimeLongClass
          else className0

        if (HijackedBoxedClasses.contains(className)) {
          if (test) {
            className match {
              case BoxedUnitClass    => expr === js.Undefined()
              case BoxedBooleanClass => typeof(expr) === "boolean"
              case BoxedByteClass    => genCallHelper("isByte", expr)
              case BoxedShortClass   => genCallHelper("isShort", expr)
              case BoxedIntegerClass => genCallHelper("isInt", expr)
              case BoxedFloatClass   => genCallHelper("isFloat", expr)
              case BoxedDoubleClass  => typeof(expr) === "number"
            }
          } else {
            className match {
              case BoxedUnitClass    => genCallHelper("asUnit", expr)
              case BoxedBooleanClass => genCallHelper("asBoolean", expr)
              case BoxedByteClass    => genCallHelper("asByte", expr)
              case BoxedShortClass   => genCallHelper("asShort", expr)
              case BoxedIntegerClass => genCallHelper("asInt", expr)
              case BoxedFloatClass   => genCallHelper("asFloat", expr)
              case BoxedDoubleClass  => genCallHelper("asDouble", expr)
            }
          }
        } else {
          js.Apply(
              envField(if (test) "is" else "as", className),
              List(expr))
        }

      case ArrayType(base, depth) =>
        js.Apply(
            envField(if (test) "isArrayOf" else "asArrayOf", base),
            List(expr, js.IntLiteral(depth)))
    }
  }

  private[javascript] def genCallHelper(helperName: String, args: js.Tree*)(
      implicit outputMode: OutputMode, pos: Position): js.Tree =
    js.Apply(envField(helperName), args.toList)

  private[javascript] def encodeClassVar(className: String)(
      implicit outputMode: OutputMode, pos: Position): js.Tree =
    envField("c", className)

  private[javascript] def envField(field: String, subField: String,
      origName: Option[String] = None)(
      implicit outputMode: OutputMode, pos: Position): js.Tree = {
    import TreeDSL._

    outputMode match {
      case OutputMode.ECMAScript51Global =>
        envField(field) DOT js.Ident(subField, origName)

      case OutputMode.ECMAScript51Isolated =>
        js.VarRef(js.Ident("$" + field + "_" + subField, origName))
    }
  }

  private[javascript] def envField(field: String)(
      implicit outputMode: OutputMode, pos: Position): js.Tree = {
    import TreeDSL._

    outputMode match {
      case OutputMode.ECMAScript51Global =>
        js.VarRef(js.Ident(ScalaJSEnvironmentName)) DOT field

      case OutputMode.ECMAScript51Isolated =>
        js.VarRef(js.Ident("$" + field))
    }
  }

  private[javascript] def envFieldDef(field: String, subField: String,
      value: js.Tree)(
      implicit outputMode: OutputMode, pos: Position): js.Tree = {
    envFieldDef(field, subField, origName = None, value)
  }

  private[javascript] def envFieldDef(field: String, subField: String,
      origName: Option[String], value: js.Tree)(
      implicit outputMode: OutputMode, pos: Position): js.Tree = {
    val globalVar = envField(field, subField, origName)

    outputMode match {
      case OutputMode.ECMAScript51Global =>
        js.Assign(globalVar, value)

      case OutputMode.ECMAScript51Isolated =>
        js.VarDef(globalVar.asInstanceOf[js.VarRef].ident, value)
    }
  }

  private[javascript] implicit class MyTreeOps(val self: js.Tree) {
    def prototype(implicit pos: Position): js.Tree =
      js.DotSelect(self, js.Ident("prototype"))
  }

  class DesugarException(tree: Tree,
      cause: Throwable) extends Exception(exceptionMsg(tree), cause)

  private def exceptionMsg(tree: Tree): String = {
    val writer = new StringWriter
    val printer = new ir.Printers.IRTreePrinter(writer)
    printer.printTopLevelTree(tree)
    "Exception while desugaring: " + writer.toString
  }
}
