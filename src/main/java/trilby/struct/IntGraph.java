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

package trilby.struct;

import trilby.struct.Unboxed.IntIterator;

public class IntGraph
{
    private IntLists in, out;
    private int max = 0;
    
    public IntGraph(boolean onHeap) {
        in = new IntLists(onHeap);
        out = new IntLists(onHeap);
    }

    public void edge(int from, int to) {
        out.add(from, to);
        in.add(to, from);
        if (from > max)
            max = from;
        if (to > max)
            max = to;
    }
    
    public IntIterator ins(int v) {
        return in.iterate(v);
    }
    
    public IntIterator outs(int v) {
        return out.iterate(v);
    }
    
    public int[] dom() {
        Dom dom = new Dom();
        int[] idom = new int[max+1];
        idom[0] = idom[1] = 0;
        for (int i = 2; i <= max; i++)
            idom[i] = dom.rev[dom.idom[dom.ord[i]]];
        return idom;
    }
    
    private class Dom {
        
        private int[] ord, rev, parent, semi, idom, ancestor, best;
        IntLists buck;
        private int num = 1;
        
        Dom() {
            ord = new int[max+1];
            rev = new int[max+1];
            parent = new int[max+1];
            semi = new int[max+1];
            idom = new int[max+1];
            ancestor = new int[max+1];
            best = new int[max+1];
            buck = new IntLists(false);
            
            buck.add(max, 0);
            buck.clear(max);
            
            // step 1
            dfs(1, 0);
            for (int v = 1; v <= max; v++) {
                semi[v] = v;
                idom[v] = 0;
                ancestor[v] = 0;
                best[v] = v;
            }
            
            for (int w = max; w > 1; w--) {
                int p = parent[w];
                
                // step 2
                // IntIterator it = ins(rev[p]); bug?
                IntIterator it = ins(rev[w]);
                for (int _v = it.next(); _v > 0; _v = it.next()) {
                    int v = ord[_v];
                    int u = eval(v);
                    // System.out.printf("%d pred %d parent %d\n", rev[v], rev[w], rev[p]);
                    if (semi[w] > semi[u])
                        semi[w] = semi[u];
                    buck.add(semi[w], w);
                    link(p, w);
                }
                
                // step 3
                it = buck.iterate(p);
                for (int v = it.next(); v > 0; v = it.next()) {
                    int u = eval(v);
                    // idom[v] = semi[u] < p ? u : p; bug?
                    idom[v] = semi[u] < semi[v] ? u : p;
                }
                buck.clear(p);
            }
            
            // step 4
            idom[1] = 0;
            for (int w = 2; w <= max; w++) {
                if (idom[w] != semi[w]) {
                    idom[w] = idom[idom[w]];
                }
            }
            
            for (int w = 1; w <= max; w++)
                System.out.printf("idom[%d] = %d\n", rev[w], rev[idom[w]]);
        }
        
        private void dfs(int _v, int _p) {
            if (ord[_v] == 0) {
                ord[_v] = num++;
                parent[ord[_v]] = ord[_p];
                // System.out.printf("parent[%d] = %d\n", _v, _p);
                rev[ord[_v]] = _v;
                IntIterator it = outs(_v);
                for (int _u = it.next(); _u > 0; _u = it.next())
                    dfs(_u, _v);
            }
        }
        
        private void link(int v, int w) {
            // System.out.printf("ancestor[%d] = %d\n", rev[w], rev[v]);
            ancestor[w] = v;
        }

        private int eval(int v) {
            if (ancestor[v] != 0) {
                compress(v);
            }
            return best[v];
        }
        
        private void compress(int v) {
            int a = ancestor[v];
            if (ancestor[a] == 0) {
                return;
            }
            // System.out.printf("compress %d, ancestor %d\n", rev[v], rev[a]);
            compress(a);
            if (semi[best[v]] > semi[best[a]]) {
                best[v] = best[a];
            }
            // System.out.printf("ancestor[%d] = %d\n", rev[v], rev[a]);
            ancestor[v] = ancestor[a];
        }
    }
}
