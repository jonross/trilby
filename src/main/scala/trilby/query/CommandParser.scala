/*
 * Copyright (c) 2012, 2013 by Jonathan Ross (jonross@alum.mit.edu)
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

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers

import trilby.hprof.Heap

class CommandParser(heap: Heap) extends RegexParsers
{
    // matches e.g. "java.lang.Object", "int[][]", "com.foo.*"

    def types: Parser[String] = 
        """[A-Za-z_][A-Za-z0-9$_]*(\.[A-Za-z_*][A-Za-z0-9$_]*)*(\[\])*""".r

    // matches a query variable name (same as java identifiers)

    def varname: Parser[String] = 
        """[A-Za-z_][A-Za-z0-9$_]*""".r

    // matches type + optional var name, e.g. "Object o"

    def vardecl =
        types ~ varname ^^ { case t ~ v => (t, Some(v)) } |
        types           ^^ { case t => (t, None) }
        
    // matches a sequences of vardecls with path operators between, e.g.
    // MyObject o -> HashMap h ->> Integer x
    // result is a List[Target]

    def pathFinder =
        vardecl ~ rep(
            "<-" ~ vardecl ^^ { case _ ~ v => Target(v._1, v._2, false, false, false) } |
            "<<-" ~ vardecl ^^ { case _ ~ v => Target(v._1, v._2, false, true, false) } |
            "->" ~ vardecl ^^ { case _ ~ v => Target(v._1, v._2, true, false, false) } |
            "->>" ~ vardecl ^^ { case _ ~ v => Target(v._1, v._2, true, true, false) } |
            "<=" ~ vardecl ^^ { case _ ~ v => Target(v._1, v._2, false, false, true) } |
            "<<=" ~ vardecl ^^ { case _ ~ v => Target(v._1, v._2, false, true, true) } |
            "=>" ~ vardecl ^^ { case _ ~ v => Target(v._1, v._2, true, false, true) } |
            "=>>" ~ vardecl ^^ { case _ ~ v => Target(v._1, v._2, true, true, true) }
        ) ^^ { case first ~ rest =>
            Target(first._1, first._2, false, false, false) :: rest
        }
    
    // matches a function call e.g. "histo x, y"
    
    def funCall =
        varname ~ arglist ^^ { case f ~ a => (f, a) }
        
    def arglist = 
        varname ~ rep(
            "," ~ varname ^^ { case _ ~ v => v }
        ) ^^ { case first ~ rest => first :: rest }
    
    // matches report function against a query, e.g.
    // histo x, y of HashMap x ->> Integer y
    
    def fullQuery =
        funCall ~ "of" ~ pathFinder ^^ {
            case f ~ _ ~ p => new GraphQuery(heap, f._1, f._2, p)
        }
    
    // Matches misc functions
    
    def miscfn =
        "skip" ~ types ^^   { case _ ~ t => () => heap.skipClasses(t, true) } |
        "skip" ^^           { case _ => () => heap.showSkippedClasses() } |
        "noskip" ~ types ^^ { case _ ~ t => () => heap.skipClasses(t, false) } |
        "noskip" ^^         { case _ => () => heap.showSkippedClasses() } |
        "set" ~ "garbage" ^^   { case _ => () => heap.hideGarbage = false } |
        "set" ~ "nogarbage" ^^ { case _ => () => heap.hideGarbage = true }
        
    def action: Parser[() => Any] =
        fullQuery ^^  { case x => x } |
        miscfn ^^     { case x => x }

    def parseCommand(text : String) = parseAll(action, text) match {
        case Error(msg, next) => sys.error(msg)
        case Failure(msg, next) => sys.error(msg)
        case p: ParseResult[() => Any] => p.get
    }
}
