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

package trilby.main

import java.io._
import org.apache.log4j.BasicConfigurator
import trilby.hprof.Heap
import trilby.hprof.HProfReader
import trilby.reports.ClassHistogram
import trilby.util.ObjectSet
import trilby.query.GraphQueryParser
import trilby.util.MappedHeapData
import trilby.util.Oddments._
import trilby.reports.GraphSearch2
import org.apache.log4j.PropertyConfigurator
import jline.console.ConsoleReader
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.Request
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import com.google.common.io.ByteStreams
import trilby.reports.FullHistogram
import trilby.nonheap.NHUtils

object Main {
    
    private var heap: Heap = null
    
    case class Options(val histogram: Boolean = false,
                       val interactive: Boolean = false,
                       // val web: Boolean = false,
                       val heapFile: File = null) {
        
        def parse(options: List[String]): Options = options match {
            case "--histo" :: _ => copy(histogram = true) parse options.tail
            case "--inter" :: _ => copy(interactive = true) parse options.tail
            // case "--web" :: _ => copy(web = true) parse options.tail
            case x :: _ if x(0) == '-' => die("Unknown option: " + x)
            case x :: Nil => copy(heapFile = new File(x))
            case _ => die("Missing or extraneous heap filenames")
        }
    }
    
    def main(args: Array[String]) = protect {
        time("Session") {
            PropertyConfigurator.configure(getClass.getResourceAsStream("log4j.properties"))
            NHUtils.initNow()
            run(Options().parse(args.toList))
        }
    }
    
    def run(options: Options) {

        val data = new MappedHeapData(options.heapFile)
        heap = new HProfReader(data).read()
        
        if (options.interactive) {
            println("Entering interactive mode")
            val useJLine = java.lang.Boolean getBoolean "trilby.use.jline"
            val readLine = if (useJLine) {
                val reader = new ConsoleReader()
                () => reader.readLine("> ")
            } else {
                () => scala.Console.readLine("> ")
            }
            val input = Stream continually { readLine() }
            for (line <- input takeWhile {_ != null} map {_.trim} filter {_.length > 0}) {
                protect { 
                    new GraphQueryParser(heap).parseCommand(line).apply() match {
                        case p: Printable =>
                            val out = new java.io.PrintWriter(System.out)
                            p.print(out)
                            out.flush()
                        case x: Unit =>
                    }
                }
            }
        }
        
        /*
        else if (options.web) {
            println("Starting web server")
            val server = new Server(7070)
            server.setHandler(new AbstractHandler {
                def handle(target: String, baseReq: Request,
                           req: HttpServletRequest, rsp: HttpServletResponse) {
                    new WebAPI(heap, baseReq, req, rsp).handle()
                }
            })
            server.start()
            server.join()
        }
        */
        
        else if (options.histogram) {
            val report = new ClassHistogram(heap, false)
            heap forEachInstance (id => {
                val classDef = heap.classes.getForObjectId(id)
                report.add(id, classDef, heap.getObjectSize(id))
            })
            val pw = new PrintWriter(System.out)
            report.print(pw)
            pw.flush()
        }
        
        else {
            die("Nothing to do")
        }
    }
}
