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

object Main {
    
    case class Options(val histogram: Boolean = false,
                       val interactive: Boolean = false,
                       val heapFile: File = null) {
        
        def parse(options: List[String]): Options = options match {
            case "--histo" :: _ => copy(histogram = true) parse options.tail
            case "--inter" :: _ => copy(interactive = true) parse options.tail
            case x :: _ if x(0) == '-' => die("Unknown option: " + x)
            case x :: Nil => copy(heapFile = new File(x))
            case _ => die("Missing or extraneous heap filenames")
        }
        
        def heap =
            new HProfReader(new MappedHeapData(heapFile)).read
    }
    
    def main(args: Array[String]) = protect {
        time("Session") {
            PropertyConfigurator.configure(getClass.getResourceAsStream("log4j.properties"))
            run(Options() parse args.toList)
        }
    }
    
    def run(options: Options) {

        val data = new MappedHeapData(options.heapFile)
        val heap = new HProfReader(data).read
        
        if (options.interactive) {
            println("Entering interactive mode")
            val input = Stream continually { scala.Console readLine "> " }
            for (line <- input takeWhile (_ != null) map (_.trim) filter (_.length > 0)) {
                protect { new GraphQueryParser(heap) parseFinder line apply }
            }
        }
        
        else if (options.histogram) {
            val report = new ClassHistogram(heap, false)
            heap forEachInstance (id => {
                val classDef = heap.classes getForObjectId id
                report.add(id, classDef, heap getObjectSize id)
            })
            val pw = new PrintWriter(System.out)
            report.render(pw)
            pw.flush()
        }
        
        else {
            die("Nothing to do")
        }
    }
}
