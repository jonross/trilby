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

package trilby.main

import java.io._
import org.apache.log4j.BasicConfigurator
import trilby.hprof.Heap
import trilby.hprof.HProfReader
import trilby.reports.ClassHistogram
import trilby.query.CommandParser
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
import trilby.nonheap.NHUtils
import org.apache.log4j.Level
import org.apache.log4j.Logger
import trilby.query.ParseException

object Main {
    
    private var heap: Heap = null
    
    def main(args: Array[String]) = protect {
        time("Session") {
            PropertyConfigurator.configure(getClass.getResourceAsStream("log4j.properties"))
            val options = Options().parse(args.toList)
            Logger.getRootLogger.setLevel(options.logLevel)
            NHUtils.initNow()
            run(options)
        }
    }
    
    def run(options: Options) {

        val data = new MappedHeapData(options.heapFile)
        heap = new HProfReader(data).read(options)
        
        if (options.interactive) {
            println("Entering interactive mode")
            val readLine = if (options.jline) {
                val reader = new ConsoleReader()
                () => reader.readLine("> ")
            } else {
                () => scala.Console.readLine("> ")
            }
            val input = Stream continually { readLine() }
            for (line <- input takeWhile {_ != null} map {_.trim} filter {_.length > 0}) {
                protect { 
                    try {
                        new CommandParser(heap).parseCommand(line).apply() match {
                            case p: Printable =>
                                val out = new java.io.PrintWriter(System.out)
                                p.print(out)
                                out.flush()
                            case x: Unit =>
                        }
                    }
                    catch {
                        case e: ParseException =>
                            System.err.println(e.getMessage + "\nPlease see " +
                                "https://github.com/jonross/trilby/wiki/Reference" +
                                " for a command summary.")
                        case e: Exception =>
                            e.printStackTrace(System.err)
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
            val report = new ClassHistogram(heap)
            for (id <- 1 to heap.maxId) {
                if (heap.canSee(id)) {
                    val classDef = heap.classes.getForObjectId(id)
                    report.add(id, classDef)
                }
            }
            val pw = new PrintWriter(System.out)
            report.print(pw)
            pw.flush()
        }
        
        else if (options.textDump) {
            heap.textDump()
        }
        
        else {
            die("Nothing to do")
        }
    }
}
