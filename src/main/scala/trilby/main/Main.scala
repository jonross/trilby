/*
 * Copyright © 2011, 2012 by Jonathan Ross (jonross@alum.mit.edu)
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
import trilby.hprof.HeapInfo
import trilby.hprof.HProfReader
import trilby.reports.ClassHistogram
import trilby.util.ObjectSet
import trilby.util.Options
import trilby.util.RawBufferedFile
import trilby.util.StreamHeapData
import trilby.query.GraphQueryParser
import trilby.util.MappedHeapData
import trilby.reports.GraphSearch2

/**
 * Command line launcher.
 * See {@link Options} for details.
 */

object Main {
    
    def main(args: Array[String]) {
        
        val options = Options.parseCommandLine(args)
        if (options == null)
            System.exit(1)
        
        BasicConfigurator.configure()

        val startTime = System.currentTimeMillis()
        
        // val file = new File(options.args.get(0))
        // val stream = new RawBufferedFile(file, 1048576)
        // val data = new StreamHeapData(stream.asDataInput, file.length)
        
        val data = new MappedHeapData(new File(options.args.get(0)))
        
        val reader = new HProfReader(data)
        val heap = reader.read()
        
        // TODO: turn off trackReferences if not needed
        
        if (options.interactive) {
            _interactiveMode(heap)
        }
        
        else if (options.histogram) {
            val report = new ClassHistogram(heap, options.showIds)
            heap.forEachInstance(id => {
                val classDef = heap.classes.getForObjectId(id)
                report.add(id, classDef, heap.getObjectSize(id))
            })
            _runReport(report)
        }
        
        else {
            System.err.println("Nothing to do")
            System.exit(1)
        }

        val endTime = System.currentTimeMillis()
        printf("Total runtime = %dms\n", endTime - startTime)
    }
    
    private def _runReport(report: { def render(pw: PrintWriter) }) {
        val pw = new PrintWriter(System.out)
        report.render(pw)
        pw.flush()
    }
    
    private def _interactiveMode(heap: HeapInfo) {
        println("Entering interactive mode")
        val in = new LineNumberReader(new InputStreamReader(System.in))
        var line: String = null
        while (true) {
            print("> ")
            line = in.readLine()
            if (line == null) {
                return
            }
            line = line.trim()
            if (line.length() > 0) {
                try {
                    val action = new GraphQueryParser(heap).parseFinder(line)
                    action()
                }
                catch {
                    case e: IOException =>
                        return
                    case e: Exception =>
                        e.printStackTrace(System.err)
                }
            }
        }
        
    }

}
