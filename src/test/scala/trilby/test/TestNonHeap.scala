package trilby.tests

import trilby.nonheap.HugeAutoArray
import org.scalatest.FunSuite
import java.util.Random

class TestNonHeap extends FunSuite {
    
    private val COUNT = 16 * 1048576
    private val random = new Random()
    
    test("byte array sanity check") {
        val a = new HugeAutoArray.OfByte(false)
        val bytes = new Array[Byte](COUNT)
        for (i <- 0 until COUNT) {
            bytes(i) = random.nextInt().toByte
            a.add(bytes(i))
        }
        for (i <- 0 until COUNT) {
            assert(a.get(i) === bytes(i))
        }
    }

    test("short array sanity check") {
        val a = new HugeAutoArray.OfShort(false)
        val shorts = new Array[Short](COUNT)
        for (i <- 0 until COUNT) {
            shorts(i) = random.nextInt().toShort
            a.add(shorts(i))
        }
        for (i <- 0 until COUNT) {
            assert(a.get(i) === shorts(i))
        }
    }

    test("int array sanity check") {
        val a = new HugeAutoArray.OfInt(false)
        val ints = new Array[Int](COUNT)
        for (i <- 0 until COUNT) {
            ints(i) = random.nextInt()
            a.add(ints(i))
        }
        for (i <- 0 until COUNT) {
            assert(a.get(i) === ints(i))
        }
        for (i <- 0 until COUNT) {
            assert(a.adjust(i, 1) === ints(i) + 1)
        }
    }

    test("long array sanity check") {
        val a = new HugeAutoArray.OfLong(false)
        val longs = new Array[Long](COUNT)
        for (i <- 0 until COUNT) {
            longs(i) = random.nextLong()
            a.add(longs(i))
        }
        for (i <- 0 until COUNT) {
            assert(a.get(i) === longs(i))
        }
    }

}
