package trilby.test

import org.scalatest.FunSuite
import trilby.util.SmallCounts
import java.util.Random
import trilby.util.BitSet

class TestStructs extends FunSuite {
    
    private val NUM_IDS = 1000000
    
    test("SmallCounts") {
        val r = new Random()
        val counts = new SmallCounts(NUM_IDS, 0.05f)
        val actuals = new Array[Int](NUM_IDS)
        for (id <- 0 until NUM_IDS) {
            val max = if (id % 50 == 0) Int.MaxValue else Byte.MaxValue
            actuals(id) = r.nextInt(max)
            counts.adjust(id, 10)
            counts.adjust(id, actuals(id) - 10)
        }
        for (id <- 0 until NUM_IDS) {
            assert(actuals(id) === counts.get(id))
        }
    }
    
    test("BitSet") {
        val r = new Random()
        val bits = new BitSet(NUM_IDS)
        val values = new Array[Boolean](NUM_IDS)
        for (i <- 0 until NUM_IDS) {
            values(i) = r.nextInt() % 2 == 0
            bits.set(i)
            if (!values(i))
                bits.clear(i)
        }
        for (i <- 0 until NUM_IDS) {
            assert(values(i) === bits.get(i))
        }
    }
}
