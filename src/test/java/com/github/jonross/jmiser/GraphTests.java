/*
 * Copyright (c) 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

package com.github.jonross.jmiser;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.github.jonross.jmiser.Settings;
import com.github.jonross.jmiser.Unboxed.IntIntVoidFn;
import com.github.jonross.jmiser.graph.Dominators;
import com.github.jonross.jmiser.graph.ImmutableIntGraph;
import com.github.jonross.jmiser.graph.IntGraph;
import com.github.jonross.jmiser.graph.MutableIntGraph;
import com.github.jonross.jmiser.graph.ImmutableIntGraph.Data;


public class GraphTests
{
    private final static Integer[][] edges_1 = {
        ints(1, 2),
        ints(2, 3, 6),
        ints(3, 5),
        ints(4),
        ints(5, 4),
        ints(6, 5)
    };
    
    private final static Integer[][] edges_2 = {
        ints(1, 2, 19, 23),
        ints(2, 3, 6),
        ints(3, 5),
        ints(4),
        ints(5, 4),
        ints(6, 5, 7),
        ints(7, 8, 9, 10),
        ints(8, 6, 16),
        ints(9, 18),
        ints(10, 11, 14, 15),
        ints(11, 12, 13),
        ints(12),
        ints(13),
        ints(14),
        ints(15),
        ints(16, 17),
        ints(17, 18),
        ints(18),
        ints(19, 20, 21, 22),
        ints(20),
        ints(22),
        ints(22),
        ints(23, 24),
        ints(24, 25, 26),
        ints(25, 26),
        ints(26, 23)
    };
    
    private final static int doms_2[] = {
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
    };
    
    @Test
    public void testIt() {
        testIt(edges_1, makeMutableGraph(edges_1), null);
        testIt(edges_2, makeMutableGraph(edges_2), doms_2);
        testIt(edges_1, makeImmutableGraph(edges_1), null);
        testIt(edges_2, makeImmutableGraph(edges_2), doms_2);
    }
    
    private IntGraph makeMutableGraph(final Integer[][] edges) {
        MutableIntGraph g = new MutableIntGraph(new Settings());
        for (Integer[] e: edges)
            for (int i = 1; i < e.length; i++)
                g.edge(e[0], e[i]);
        return g;
    }
    
    private IntGraph makeImmutableGraph(final Integer[][] edges) {
        Data data = new Data() {
            public void edges(IntIntVoidFn fn) {
                for (Integer[] e: edges)
                    for (int i = 1; i < e.length; i++)
                        fn.apply(e[0], e[i]);
            }
        };
        return new ImmutableIntGraph(data, new Settings());
    }
    
    private void testIt(Integer[][] edges, IntGraph g, int[] doms) {
        
        boolean up = g instanceof MutableIntGraph;
        
        for (Integer[] e: edges) {
            int i = up ? 1 : e.length-1;
            for (long cur = g.walkOutEdges(e[0]); cur != 0; cur = g.nextOutEdge(cur)) {
                assertEquals((int) e[i], (int) (cur & 0xFFFFFFFFL));
                i += up ? 1 : -1;
            }
            assertEquals(i, up ? e.length : 0);
        }
        
        if (doms == null)
            return;
        
        Dominators d = new Dominators(g);
        int[] idom = d.get();
        for (int i = 1; i < doms.length; i++)
            assertEquals(doms[i], idom[i]);
        
        d.destroy();
        g.destroy();
    }
    
    private static Integer[] ints(Integer...ints) {
        return ints;
    }
}
