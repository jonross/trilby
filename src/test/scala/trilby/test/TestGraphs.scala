package trilby.test

import org.scalatest.FunSuite
import trilby.graph.MutableIntGraph
import trilby.graph.IntGraph
import trilby.graph.CompactIntGraph
import trilby.graph.Dominators
import trilby.graph.Dominators
import trilby.graph.Dominators
import trilby.graph.DominatorsOG

class TestGraphs extends FunSuite {
    
    private[this] val edges_1 = Array(
        Array(1, 2),
        Array(2, 3, 6),
        Array(3, 5),
        Array(4),
        Array(5, 4),
        Array(6, 5)
    )
    
    private[this] val edges_2 = Array(
        Array(1, 2, 19, 23),
        Array(2, 3, 6),
        Array(3, 5),
        Array(4),
        Array(5, 4),
        Array(6, 5, 7),
        Array(7, 8, 9, 10),
        Array(8, 6, 16),
        Array(9, 18),
        Array(10, 11, 14, 15),
        Array(11, 12, 13),
        Array(12),
        Array(13),
        Array(14),
        Array(15),
        Array(16, 17),
        Array(17, 18),
        Array(18),
        Array(19, 20, 21, 22),
        Array(20),
        Array(22),
        Array(22),
        Array(23, 24),
        Array(24, 25, 26),
        Array(25, 26),
        Array(26, 23)
    )
    
    private[this] val doms_2 = Array(
        -1,
        0,
        1,
        2,
        5,
        2,      // 5
        2,
        6,
        7,
        7,
        7,      // 10
        10,
        11,
        11,
        10,
        10,     // 15
        8,
        16,
        7,
        1,
        19,     // 20
        19,
        19,
        1,
        23,
        24,      // 25
        24
    )
    
    test("graphs") {
        testIt(edges_1, makeMutableGraph(edges_1), null)
        testIt(edges_2, makeMutableGraph(edges_2), doms_2)
        testIt(edges_1, makeCompactGraph(edges_1, 6), null)
        testIt(edges_2, makeCompactGraph(edges_2, 26), doms_2)
    }
    
    private def makeMutableGraph(edges: Array[Array[Int]]) = {
        val g = new MutableIntGraph(true)
        for (e <- edges)
            for (i <- 1 until e.length)
                g.edge(e(0), e(i))
        g
    }
    
    private def makeCompactGraph(edges: Array[Array[Int]], maxNode: Int) = {
        val f = (g: (Int, Int) => Unit) => {
            for (e <- edges)
                for (i <- 1 until e.length)
                    g(e(0), e(i))
        }
        new CompactIntGraph(maxNode, f, true)
    }
    
    private def testIt(edges: Array[Array[Int]], g: IntGraph, doms: Array[Int]) {
        
        val up = g.isInstanceOf[MutableIntGraph]
        
        for (e <- edges) {
            var i = if (up) 1 else e.length - 1
            var cur = g.walkOutEdges(e(0))
            while (cur != 0) {
            // for (long cur = g.walkOutEdges(e[0]); cur != 0; cur = g.nextOutEdge(cur)) {
                assert(e(i) === (cur & 0xFFFFFFFFL).toInt)
                i = if (up) i + 1 else i - 1
                cur = g.nextOutEdge(cur)
            }
            assert(i === (if (up) e.length else 0))
        }
        
        if (doms == null)
            return;
        
        val d = new Dominators(g)
        var idom = d.get()
        for (i <- 1 until doms.length) {
            assert(doms(i) === idom(i))
        }
        d.free()
        
        val domGraph = DominatorsOG.apply(g, 1, true)
        for (v <- 1 until doms.length) {
            var cur = domGraph.walkInEdges(v)
            if (doms(v) == 0) {
                assert(cur == 0)
            }
            else {
                val w = (cur & 0xFFFFFFFFL).asInstanceOf[Int]
                assert(domGraph.nextInEdge(cur) == 0)
                assert(w == doms(v))
            }
        }
        
        g.free()
    }
}