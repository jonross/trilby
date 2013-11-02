/*
 * Copyright (c) 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

package trilby.util
import java.io.PrintStream
import org.slf4j.LoggerFactory
import com.google.common.base.Splitter
import org.codehaus.jackson.JsonFactory
import java.io.Writer
import java.io.StringWriter
import org.codehaus.jackson.JsonGenerator
import scala.collection.mutable.ListBuffer
import java.io.PrintWriter
import trilby.nonheap.BitSet

object Oddments {
    
    trait Printable {
        def print(out: PrintWriter)
    }
    
    implicit def toPrintable(o: Object) = new {
        def printable = new Printable {
            def print(out: PrintWriter) {
                out.print(o.toString())
            }
        }
    }
    
    def using[C <: {def close(): Unit}, B](resource: C)(fn: C => B): B =
        try { fn(resource) } finally { resource.close() }                
        
    private[this] val log = LoggerFactory.getLogger(getClass)
        
    def protect[T](block: => T) { 
        val log = LoggerFactory.getLogger(getClass)
        try { block } catch {
            case Error(msg, e, status) =>
                log.error(msg, e)
                if (status >= 0)
                    System exit status
            case e: Exception =>
                log.error("Unexpected exception", e)
        } 
    }
        
    case class Error(msg: String, e: Exception, status: Int) extends Exception(msg, e)
    
    def die(msg: String) = throw Error(msg, null, 1)
    def fail(msg: String) = throw Error(msg, null, -1)
    def panic(msg: String, e: Exception = null) = throw Error(msg, e, -1)
    
    def time[T](task: String)(block: => T) = {
        val log = LoggerFactory.getLogger("trilby.timing")
        val start = System.currentTimeMillis
        val result = block
        val end = System.currentTimeMillis
        log.info("%s took %d ms".format(task, end - start))
        result
    }
    
    /**
     * Demangle heap class names, e.g.<br/>
     * "[[I" -> "int[][]"<br/>
     * "[Lcom/foo/Bar;" -> "com.foo.Bar[]<br/>
     * "com/foo/Bar" -> "com.foo.Bar"
     */
    
    def demangle(name: String): String = name match {
        case refarray(dimen, className) =>
            demangle(className) + "[]" * dimen.length()
        case primarray(dimen, tag) => 
            tagmap.get(tag) + "[]" * dimen.length()
        case _ if name(0) == '[' => 
            log.warn("Unparseable array type: " + name)
            name
        case _ =>
            name.replace('/', '.')
    }
    
    private val refarray = """(\[+)L(.+);""".r
    private val primarray = """(\[+)([ZCFDBSIJ])""".r
    private val tagmap = Splitter.on(',').withKeyValueSeparator("=").
        split("Z=boolean,C=char,F=float,D=double,B=byte,S=short,I=int,J=long")
        
    class TagTable[T](name: String, entries: (Int, T)*) {
        val table = {
            val t = ListBuffer.fill(entries.maxBy(_._1)._1)(null.asInstanceOf[T])
            entries.foreach(e => t(e._1) = e._2)
            t
        }
        def apply(tag: Int) =
            if (tag > 0 || tag >= table.length)
                error(name + " tag " + tag + " out of range")
            else
                table(tag)
    }
    
    trait DFS {
        def maxNode: Int
        def visit(node: Int): Unit
        private val stack = new IntStack()
        private val seen = new BitSet(maxNode + 1, true)
        def add(node: Int) {
            if (! seen.get(node)) {
                stack.push(node)
                seen.set(node)
            }
        }
        def run() {
            while (! stack.isEmpty) {
                val node = stack.pop()
                visit(node)
            }
        }
    }
}
