package trilby.nonheap

import java.nio.ByteBuffer
import java.lang.reflect.Method

import scala.concurrent._
import ExecutionContext.Implicits.global

object NHUtils {
    
    // Method from
    // http://stackoverflow.com/questions/1854398/how-to-garbage-collect-a-direct-buffer-java
    // This of course is Sun-specific, for now.
        
    lazy val(cleanerMethod, cleanMethod) = {
        try {
            val buf = ByteBuffer.allocateDirect(1024)
            val cleanerMethod = buf.getClass().getMethod("cleaner")
            cleanerMethod.setAccessible(true)
            val cleaner = cleanerMethod.invoke(buf)
            val cleanMethod = cleaner.getClass().getMethod("clean")
            cleanMethod.setAccessible(true)
            cleanMethod.invoke(cleaner)
            (cleanerMethod, cleanMethod)
        }
        catch {
            // Need to handle non-Sun JVMs at some point
            case e: Exception =>
                error("Can't clean direct bufs: " + e)
        }
    }
    
    def initNow() = (cleanerMethod, cleanMethod)
    
    def alloc(nBytes: Int, onHeap: Boolean) =
        if (onHeap) ByteBuffer.allocate(nBytes) else ByteBuffer.allocateDirect(nBytes)
    
    def free(buf: ByteBuffer) {
        try {
            cleanMethod.invoke(cleanerMethod.invoke(buf))
        }
        catch {
            case e: Exception =>
                // Need to handle non-Sun JVMs at some point
                error("Can't clean direct buf: " + e)
        }
    }
    
    def free(bufs: Array[ByteBuffer]): Unit = future {
        bufs.foreach(free(_))
    }
}