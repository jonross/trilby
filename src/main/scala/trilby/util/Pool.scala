package trilby.util

import java.util.concurrent.LinkedBlockingQueue

/**
 * A simple pool holding a small number of expensive objects, created on-demand.
 */

abstract class Pool[T](size: Int) {
    
    def create(): T
    def destroy(t: T): Unit
    
    private[this] val queue = new LinkedBlockingQueue[Option[T]]()
    
    for (i <- 1 to size)
        queue.put(None)
    
    def alloc() =
        queue.take().getOrElse(create())
        
    def free(item: T) = 
        queue.put(Some(item))
        
    def free() =
        for (i <- 1 to size)
            queue.take().map(destroy(_))
}