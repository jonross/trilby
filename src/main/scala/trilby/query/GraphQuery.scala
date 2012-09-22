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

import trilby.hprof.HeapInfo
import trilby.reports.ClassHistogram
import trilby.reports.GraphSearch2
import trilby.util.Oddments._
import java.io.PrintWriter

/**
 * A complete query spec for the {@link GraphSearch} engine.
 */

trait Renderable {
    type T
    def render(pw: PrintWriter): Unit
    def +(that: T): T
}

class GraphQuery(heap: HeapInfo, funName: String, funArgs: List[String],
                 val finder: List[Target]) extends (() => Unit) {
    
    private val functions: Map[String,(Int, () => QueryFunction with Renderable)] = 
        Map("histo" -> ((2, () => new ClassHistogram(heap))))
        
    def apply() { new GraphSearch2(heap, this).run() }
    
    /** Function to accept object IDs from the search path */
    val acceptor = functions.get(funName) match {
        case Some((nargs, maker)) =>
            if (nargs != funArgs.size)
                fail(funName + " takes " + nargs + " arguments")
            maker()
        case None =>
            fail("Unknown function: " + funName)
    }
    
    /** Indices of function arguments in the path expression */
    val argIndices = funArgs.toArray.map(arg =>
        finder.indexWhere(_.varName.getOrElse("") == arg) match {
            case -1 => fail("No arg named " + arg + " defined in path")
            case index @ _ => index
        }
    )
}

/**
 * Represents one step in a path finder, e.g. "-> ArrayList list"
 */

case class Target(/** The type name / wildcard, e.g. "ArrayList" */
                  types: String,
                  /** Optional variable name, e.g. "list" */
                  varName: Option[String],
                  /** Follow refs to this node; true if "->", false if "<-" */
                  to: Boolean, 
                  /** Skip instances of elidable classes */
                  elide: Boolean)
    
/**
 * Implemented by classes that can receive a set of object IDs at
 * one point in a graph path.
 */

trait QueryFunction {
    def accept(ids: Array[Int]): Unit
}
