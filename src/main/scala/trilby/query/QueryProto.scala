/*
 * Copyright © 2012 by Jonathan Ross (jonross@alum.mit.edu)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package trilby.query

import scala.collection.{mutable, immutable}
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.Parsers

/**
 * Imported from demo-scala for raw material.
 * Development continuing there.
 */

object HelCalc
{
    trait Showable { def show(): String }
    
    type LL_L = (Long, Long) => Long
    type LL_B = (Long, Long) => Boolean 
    type L_L = Long => Long
    type DD_D = (Double, Double) => Double
    type DD_B = (Double, Double) => Boolean
    type D_D = (Double) => Double
    type BB_B = (Boolean, Boolean) => Boolean
    type B_B = Boolean => Boolean
    
    abstract sealed class Op(name: String) extends Showable {
        def show() = name
    }
    
    abstract sealed class BinaryNumericOp(name: String, val longFn: LL_L, val doubleFn: DD_D)
        extends Op(name)
    
    case object ADD extends BinaryNumericOp("+", _ + _, _ + _)
    case object SUB extends BinaryNumericOp("-", _ - _, _ - _)
    case object MUL extends BinaryNumericOp("*", _ * _, _ * _)
    case object DIV extends BinaryNumericOp("/", _ / _, _ / _)
    
    abstract sealed class BinaryIntegerOp(name: String, val longFn: LL_L) extends Op(name)
    
    case object MOD extends BinaryIntegerOp("%", _ % _)
    case object LSH extends BinaryIntegerOp("<<", _ << _)
    case object RSH extends BinaryIntegerOp(">>", _ >> _)
    case object BIT_AND extends BinaryIntegerOp("&", _ & _)
    case object BIT_OR extends BinaryIntegerOp("|", _ | _)
    
    abstract sealed class UnaryIntegerOp(name: String, val longFn: L_L) extends Op(name)
    
    case object BIT_NOT extends UnaryIntegerOp("~", ~_)
    case object NEG extends UnaryIntegerOp("-", -_)
    
    abstract sealed class BinaryBooleanOp(name: String, val boolFn: BB_B) extends Op(name)
    
    case object BOOL_AND extends BinaryBooleanOp("&&", _ && _)
    case object BOOL_OR extends BinaryBooleanOp("||", _ || _)
    
    abstract sealed class UnaryBooleanOp(name: String, val boolFn: B_B) extends Op(name)
    
    case object BOOL_NOT extends UnaryBooleanOp("!", !_)
    
    abstract sealed class BooleanNumericOp(name: String, longFn: LL_B, doubleFn: DD_B) 
        extends Op(name)
    
    case object LT extends BooleanNumericOp("<", _ < _, _ < _)
    case object LE extends BooleanNumericOp("<=", _ <= _, _ <= _)
    case object GT extends BooleanNumericOp(">", _ > _, _ > _)
    case object GE extends BooleanNumericOp(">=", _ >= _, _ >= _)
    case object EQ extends BooleanNumericOp("==", _ == _, _ == _)
    case object NE extends BooleanNumericOp("!=", _ != _, _ != _)
    
    abstract sealed class GraphSearchOp(name: String) extends Op(name)
    
    case object FROM extends GraphSearchOp("<-")
    case object FROM_X extends GraphSearchOp("<<-")
    case object TO extends GraphSearchOp("->")
    case object TO_X extends GraphSearchOp("->>")
    
    /*
     * AST expressions.  These are untyped at parse time and later validated / converted
     * into typed forms for evaluation.
     */
    
    object AST {
        
        abstract sealed class Node {
            def show(): String
            def eval(context: Context): Any
        }
        
        // Assignment
        
        case class Assignment(name: String, expr: Node) extends Node {
            def show() = name + " = " + expr.show()
            def eval(context: Context) = {
                if (context.get(name) != null)
                    sys.error("Variable " + name + " is already bound")
                context.put(name, expr.eval(context))
            }
        }
        
        // Numeric valued nodes
        
        abstract sealed class N_Node extends Node {
            def evalLong(): Long
            def evalDouble(): Double
        }
        
        abstract sealed class L_Node(fn: () => Long) extends N_Node {
            def evalLong() = fn()
            def evalDouble() = evalLong().toDouble
            def eval(context: Context) = evalLong().asInstanceOf[Any]
        }
        
        abstract sealed class D_Node(fn: () => Double) extends N_Node {
            def evalLong() = evalDouble().toLong
            def evalDouble() = fn()
            def eval(context: Context) = Double.box(evalDouble())
        }
        
        // Constant numeric values + builder for var refs
        
        case class L_Const(value: Long) extends L_Node(() => value) {
            def show() = value.toString()
        }
        
        case class D_Const(value: Double) extends D_Node(() => value) {
            def show() = value.toString()
        }
        
        def buildConst(context: Context, name: String) = context.get(name) match {
            case null =>
                sys.error("Unbound variable: " + name)
            case x: Long =>
                L_Const(x)
            case x: Double =>
                D_Const(x)
            case _ =>
                sys.error("Unable to use " + name)
        }
        
        // Unary expressions + builder
        
        trait UnaryExpr {
            val op: Op
            val that: Node
            def show() = op + " " + that.show()            
        }
        
        case class L_UnaryExpr(val op: UnaryIntegerOp, that: N_Node) 
            extends L_Node(() => op.longFn(that.evalLong())) with UnaryExpr
        
        def buildUnaryExpr(op: Op, node: Node) = (op, node) match {
            case (numop: UnaryIntegerOp, lnode: L_Node) =>
                L_UnaryExpr(numop, lnode)
            case (_, _) =>
                sys.error("type clash: " + op + " " + node)
        }
        
        // Binary expressions + builder
        
        trait BinaryExpr {
            val op: Op
            val left: Node
            val right: Node
            def show() = left.show() + " " + op + " " + right.show()            
        }
        
        case class L_BinaryExpr(op: BinaryIntegerOp, left: N_Node, right: N_Node) 
            extends L_Node(() => op.longFn(left.evalLong(), right.evalLong())) 
            with BinaryExpr
            
        case class L_BinaryExpr2(op: BinaryNumericOp, left: N_Node, right: N_Node) 
            extends L_Node(() => op.longFn(left.evalLong(), right.evalLong())) 
            with BinaryExpr
            
        case class D_BinaryExpr(op: BinaryNumericOp, left: N_Node, right: N_Node) 
            extends D_Node(() => op.doubleFn(left.evalDouble(), right.evalDouble())) 
            with BinaryExpr
            
        def buildBinaryExpr(op: Op, left: Node, right: Node) = (op, left, right) match {
            // LHS and RSH are long, op is integral
            case (numop: BinaryIntegerOp, lleft: L_Node, lright: L_Node) =>
                L_BinaryExpr(numop, lleft, lright)
            // LHS and RHS are long, op either, create L_Node
            case (numop: BinaryNumericOp, lleft: L_Node, lright: L_Node) =>
                L_BinaryExpr2(numop, lleft, lright)
            // LHS or RHS is long, op either, cast to Double
            case (numop: BinaryNumericOp, lleft: N_Node, lright: N_Node) =>
                D_BinaryExpr(numop, lleft, lright)
            // TODO: handle more cases
            case (_, _, _) =>
                sys.error("Type clash: " + left + " " + op + " " + right)
        }
        
        // Boolean valued node
        
        abstract sealed class B_Node(fn: () => Boolean) extends Node {
            def eval(context: Context) = fn().asInstanceOf[Any] // todo: fix
        }
        
        case class B_Const(value: Boolean) extends B_Node(() => value) {
            def show() = value.toString()
        }
        
        // Graph search
        
        case class Search(name: String, expr: GraphExpr, cond: B_Node) extends Node {
            def eval(context: Context) = sys.error("nyi")
            def show() = sys.error("nyi")
        }
        
        case class GraphExpr(name: String) extends Node {
            def eval(context: Context) = sys.error("nyi")
            def show() = sys.error("nyi")
        }
    
    }
    
    class AnyParser(context: Context) extends JavaTokenParsers
    {
        import AST._
        
        def directive: Parser[Node] =
            assignment ^^ { case x => x } |
            anyExpr ^^ { case x => x }
            
        def assignment =
            ident ~ "=" ~ anyExpr ^^ { case id ~ op ~ rhs => Assignment(id, rhs) }
            
        def anyExpr = searchExpr | addExpr
        
        def searchExpr =
            ident ~ "from" ~ graphExpr ~ "where" ~ boolExpr ^^ {
                case id ~ _ ~ g ~ _ ~ cond => Search(id, g, cond)
            }
        
        def graphExpr = // TODO: more 
            ident ^^ { case id => GraphExpr(id) }
        
        def boolExpr = // TODO: more
            "true" ^^ { case _ => B_Const(true) } |
            "false" ^^ { case _ => B_Const(true) }
        
        def addExpr: Parser[Node] = mulExpr ~ rep(
            "+" ~ mulExpr ^^ { case op ~ rhs => (ADD, rhs) } | 
            "-" ~ mulExpr ^^ { case op ~ rhs => (SUB, rhs) }
        ) ^^ { case lhs ~ rhs => rhs.foldLeft(lhs)((lhs, oprhs) => buildBinaryExpr(oprhs._1, lhs, oprhs._2)) }

        def mulExpr: Parser[Node] = factor ~ rep(
            "*" ~ factor ^^ { case op ~ rhs => (MUL, rhs) } | 
            "/" ~ factor ^^ { case op ~ rhs => (DIV, rhs) }
        ) ^^ { case lhs ~ rhs => rhs.foldLeft(lhs)((lhs, oprhs) => buildBinaryExpr(oprhs._1, lhs, oprhs._2)) }

        def factor: Parser[Node] = 
            "(" ~> addExpr <~ ")" ^^ { case x => x } |
            floatingPointNumber ^^ { case x => D_Const(x.toFloat) } |
            ident ^^ { case id => buildConst(context, id)}
      
        def parse(text : String) = { 
            val ast = parseAll(directive, text)
            ast match {
                case Error(msg, next) => sys.error(msg)
                case Failure(msg, next) => sys.error(msg)
                case p: ParseResult[Node] => p
            }
        }
    }
    
    val expressions = List(
        "5",
        "(5)",
        "5 + 5",
        "(5 + 5)",
        "5 + 5 + 5",
        "5 / 5 + 5 * 5",
        "(5 + 5) + 5",
        "(5 * 5) / (5 * 5)",
        "5 - 5",
        "5 - 5 + 5",
        "5 - (5 + 5)",
        "5 * 5 / 5",
        "5 / 5 / 5",
        "x = 1",
        "x + 5"
        )
        
    class Context {
        private[this] val map = mutable.HashMap[String,Any]()
        def get(name: String) = map.getOrElse(name, null)
        def put(name: String, value: Any) = map.put(name, value)
    }
        
    def main(args: Array[String]) {
        val c = new Context()
        for (x <- expressions) {
            val ast = new AnyParser(c).parse(x).get
            println(ast.getClass().getName())
            println(x + " = " + ast.show() + " = " + ast.eval(c))
        }
    }
}
