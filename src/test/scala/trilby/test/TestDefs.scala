package trilby.test

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers._
import trilby.hprof.ClassInfo
import trilby.hprof.Field
import trilby.util.BitSet
import trilby.util.Oddments.Options
    
class TestDefs extends FunSuite {
    
    test("matching classes") {
        val info = defs
        val bits = new BitSet(10)
        def verify(hids: Int*) {
            hids.foreach(hid => bits(hid) || fail("hid " + hid + " not set"))
            (1 to 7).toList.diff(hids).foreach(hid => bits(hid) && fail ("hid " + hid + " set"))
        }
        verify()
        info.matchClasses(bits, "java.lang.Object", true)
        verify(1, 2, 3, 4, 5, 6, 7)
        info.matchClasses(bits, "java.lang.Number", false)
        verify(1, 2, 6, 7)
        info.matchClasses(bits, "java.util.*", false)
        verify(1, 2)
    }
        
    private def defs = {
        val ci = new ClassInfo(Options())
        for ((name, hid, superHid) <- List(
            ("java.lang.Object", 1, 0),
            ("java.lang.String", 2, 1),
            ("java.lang.Number", 3, 1),
            ("java.lang.Integer", 4, 3),
            ("java.lang.Long", 5, 3),
            ("java.util.List", 6, 1),
            ("java.util.Map", 7, 1)
            )) 
        {
            ci.addClassDef(name, hid, superHid, new Array[Field](0))
        }
        for (c <- ci) c.cook()
        ci
    }
}