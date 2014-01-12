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
import org.apache.log4j.Level
import java.io.File

object Oddments {
    
    case class Options(val dominators: Boolean = true,
                       val histogram: Boolean = false,
                       val interactive: Boolean = true,
                       val jline: Boolean = false,
                       val logLevel: Level = Level.WARN,
                       val onHeap: Boolean = true,
                       val textDump: Boolean = false,
                       // val web: Boolean = false,
                       val heapFile: File = null) {
        
        def parse(options: List[String]): Options = options match {
            case "--debug" :: _ => copy(logLevel = Level.DEBUG) parse options.tail
            case "--histo" :: _ => copy(histogram = true, interactive = false) parse options.tail
            case "--info" :: _ => copy(logLevel = Level.INFO) parse options.tail
            case "--jline" :: _ => copy(jline = true) parse options.tail
            case "--nodom" :: _ => copy(dominators = false) parse options.tail
            case "--offheap" :: _ => copy(onHeap = false) parse options.tail
            case "--textdump" :: _ => copy(textDump = true, interactive = false) parse options.tail
            // case "--web" :: _ => copy(web = true) parse options.tail
            case x :: _ if x(0) == '-' => die("Unknown option: " + x)
            case x :: Nil => copy(heapFile = new File(x))
            case _ => die("Missing or extraneous heap filenames")
        }
    }

    class IntCursor(val self: Long) extends AnyVal {
        @inline def valid = self != 0
        @inline def value = (self & 0xFFFFFFFFL).asInstanceOf[Int]
        @inline def position = ((self >>> 32) & 0xFFFFFFFFL).asInstanceOf[Int]
    }
    
    object IntCursor {
        def apply(position: Int, value: Int) = 
            new IntCursor((position.toLong << 32) | value)
    }
    
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
        
    trait DFS {
        def maxNode: Int
        def visit(node: Int): Unit
        def addChildren(node: Int): Unit
        protected val stack = new IntStack()
        private val seen = new BitSet(maxNode + 1)
        def add(node: Int) =
            if (seen(node)) false else {
                stack.push(node)
                seen.set(node)
                true
            }
        def add(extra: Int, node: Int) = {
            if (seen(node)) false else {
                stack.push(extra)
                stack.push(node)
                seen.set(node)
                true
            }
        }
    }
    
    trait PreorderDFS extends DFS {
        def run() {
            while (! stack.isEmpty) {
                val node = stack.pop()
                visit(node)
                addChildren(node)
            }
        }
    }
    
    trait PostorderDFS extends DFS {
        def run() {
            while (! stack.isEmpty) {
                val node = stack.pop()
                if (node > 0) {
                    stack.push(-node)
                    addChildren(node)
                }
                else {
                    visit(-node)
                }
            }
        }
    }
}
