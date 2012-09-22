/*
 * Copyright © 2012 by Jonathan Ross (jonross@alum.mit.edu)
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

package trilby.util;

import java.util.ArrayList;
import java.util.List;

import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;
    
public class Options {
        
    @Option(name="--depth", usage="depth for recursive reports like -h, default 1")
    public int depth = 1;
        
    @Option(name="--holders", usage="run holders report on specified class")
    public String holders = null;
        
    @Option(name="--histo", usage="run class histogram report")
    public boolean histogram = false;
        
    @Option(name="--showids", usage="show instance / class ids in reports")
    public boolean showIds = false;
    
    @Option(name="--inter", usage="interactive query mode")
    public boolean interactive = false;
        
    @Argument
    public List<String> args = new ArrayList<String>();
    
    public static Options parseCommandLine(String[] argv)
    {
        Options options = new Options();
        CmdLineParser parser = new CmdLineParser(options);
        
        try {
            parser.parseArgument(argv);
            if (options.args.size() != 1)
                throw new CmdLineException(parser, "Missing heap filename");
            return options;
        }
        catch (CmdLineException e) {
            System.err.print(e.getMessage());
            parser.printUsage(System.err);
            return null;
        }
    }
}
