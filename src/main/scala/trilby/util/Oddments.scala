/*
 * Copyright (c) 2011, 2012 by Jonathan Ross (jonross@alum.mit.edu)
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
import com.google.common.base.Splitter
import java.io.PrintStream

object Oddments {
    
    def protect[T](block: => T) { try { block } catch {
        case Error(msg, e, status) =>
            warn(msg)
            if (e != null)
                e printStackTrace System.err
            if (status >= 0)
                System exit status
        case e: Exception =>
            warn("Unexpected exception")
            e printStackTrace System.err
    } }
    
    case class Error(msg: String, e: Exception, status: Int) extends Exception(msg, e)
    
    def die(msg: String) = throw Error(msg, null, 1)
    def fail(msg: String) = throw Error(msg, null, -1)
    def panic(msg: String, e: Exception = null) = throw Error(msg, e, -1)
    
    def time[T](task: String)(block: => T) = {
        val start = System.currentTimeMillis
        val result = block
        val end = System.currentTimeMillis
        info("%s took %d ms", task, end - start)
        result
    }
    
    // TODO: use logger
    
    def info(fmt: String, args: Any*) = output(System.out, fmt.format(args: _*))
    def warn(fmt: String, args: Any*) = output(System.err, fmt.format(args: _*))
    
    def output(out: PrintStream, s: String) =
        out.print(s)
        
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
            warn("Unparseable array type: " + name)
            name
        case _ =>
            name.replace('/', '.')
    }
    
    private val refarray = """(\[+)L(.+);""".r
    private val primarray = """(\[+)([ZCFDBSIJ])""".r
    private val tagmap = Splitter.on(',').withKeyValueSeparator("=").
        split("Z=boolean,C=char,F=float,D=double,B=byte,S=short,I=int,J=long")
}
