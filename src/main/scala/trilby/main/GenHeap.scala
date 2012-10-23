package trilby.main
import java.util.Random
import java.util.ArrayList
import java.util.HashMap

object GenHeap {
    
    val map = new HashMap[Int,ArrayList[String]]
    
    def main(args: Array[String]) {
        
        val count = args(0).toInt
        val rand = new Random()
        var list = new ArrayList[String]()
        
        for (i <- 1 to count) {
            list.add(String.valueOf(i))
            if (rand.nextInt(10) == 0) {
                map.put(i, list)
                list = new ArrayList[String]()
            }
        }
        
        println("Sleeping")
        Thread.sleep(60 * 1000)
    }

}
