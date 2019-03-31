<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html lang = "en">

<head>

<link rel="shortcut icon" href="https://algs4.cs.princeton.edu/favicon.ico">
<link rel="stylesheet"    href="https://algs4.cs.princeton.edu/java.css" type="text/css">

<title>Quick3way.java</title>

<meta HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=iso-8859-1">
<meta NAME="AUTHOR" CONTENT="Robert Sedgewick and Kevin Wayne">
<meta NAME="DESCRIPTION" CONTENT="Quick3way code in Java">
<meta NAME="TITLE" CONTENT="Quick3way code in Java">
<meta NAME="KEYWORDS" CONTENT="Quick3way,java,programming,computer science,algorithm,data structure,program,code">
<meta NAME="ROBOTS" CONTENT="INDEX,FOLLOW">

</head>


<body>
<center><h1>Quick3way.java</h1></center><p><br>

Below is the syntax highlighted version of <a href = "Quick3way.java">Quick3way.java</a>.
<p><br>

<!-- Generator: GNU source-highlight 3.1.6
by Lorenzo Bettini
http://www.lorenzobettini.it
http://www.gnu.org/software/src-highlite -->
<pre><tt><span class="comment">/******************************************************************************</span>
<span class="comment"> *  Compilation:  javac Quick3way.java</span>
<span class="comment"> *  Execution:    java Quick3way &lt; input.txt</span>
<span class="comment"> *  Dependencies: StdOut.java StdIn.java</span>
<span class="comment"> *  Data files:   </span><span class="url">https://algs4.cs.princeton.edu/23quicksort/tiny.txt</span>
<span class="comment"> *                </span><span class="url">https://algs4.cs.princeton.edu/23quicksort/words3.txt</span>
<span class="comment"> *   </span>
<span class="comment"> *  Sorts a sequence of strings from standard input using 3-way quicksort.</span>
<span class="comment"> *   </span>
<span class="comment"> *  % more tiny.txt</span>
<span class="comment"> *  S O R T E X A M P L E</span>
<span class="comment"> *</span>
<span class="comment"> *  % java Quick3way &lt; tiny.txt</span>
<span class="comment"> *  A E E L M O P R S T X                 [ one string per line ]</span>
<span class="comment"> *    </span>
<span class="comment"> *  % more words3.txt</span>
<span class="comment"> *  bed bug dad yes zoo ... all bad yet</span>
<span class="comment"> *  </span>
<span class="comment"> *  % java Quick3way &lt; words3.txt</span>
<span class="comment"> *  all bad bed bug dad ... yes yet zoo    [ one string per line ]</span>
<span class="comment"> *</span>
<span class="comment"> ******************************************************************************/</span>

<span class="preproc">package</span><span class="normal"> edu</span><span class="symbol">.</span><span class="normal">princeton</span><span class="symbol">.</span><span class="normal">cs</span><span class="symbol">.</span><span class="normal">algs4</span><span class="symbol">;</span>

<span class="comment">/**</span>
<span class="comment"> *  The {</span><span class="type">@code</span><span class="comment"> Quick3way} class provides static methods for sorting an</span>
<span class="comment"> *  array using quicksort with 3-way partitioning.</span>
<span class="comment"> *  </span><span class="keyword">&lt;p&gt;</span>
<span class="comment"> *  For additional documentation,</span>
<span class="comment"> *  see </span><span class="keyword">&lt;a</span><span class="normal"> </span><span class="type">href</span><span class="symbol">=</span><span class="string">"https://algs4.cs.princeton.edu/23quick"</span><span class="keyword">&gt;</span><span class="comment">Section 2.3</span><span class="keyword">&lt;/a&gt;</span><span class="comment"> of</span>
<span class="comment"> *  </span><span class="keyword">&lt;i&gt;</span><span class="comment">Algorithms, 4th Edition</span><span class="keyword">&lt;/i&gt;</span><span class="comment"> by Robert Sedgewick and Kevin Wayne.</span>
<span class="comment"> *</span>
<span class="comment"> *  </span><span class="type">@author</span><span class="comment"> Robert Sedgewick</span>
<span class="comment"> *  </span><span class="type">@author</span><span class="comment"> Kevin Wayne</span>
<span class="comment"> */</span>
<span class="keyword">public</span><span class="normal"> </span><span class="keyword">class</span><span class="normal"> </span><span class="classname">Quick3way</span><span class="normal"> </span><span class="cbracket">{</span>

<span class="normal">    </span><span class="comment">// This class should not be instantiated.</span>
<span class="normal">    </span><span class="keyword">private</span><span class="normal"> </span><span class="function">Quick3way</span><span class="symbol">()</span><span class="normal"> </span><span class="cbracket">{</span><span class="normal"> </span><span class="cbracket">}</span>

<span class="normal">    </span><span class="comment">/**</span>
<span class="comment">     * Rearranges the array in ascending order, using the natural order.</span>
<span class="comment">     * </span><span class="type">@param</span><span class="comment"> a the array to be sorted</span>
<span class="comment">     */</span>
<span class="normal">    </span><span class="keyword">public</span><span class="normal"> </span><span class="keyword">static</span><span class="normal"> </span><span class="type">void</span><span class="normal"> </span><span class="function">sort</span><span class="symbol">(</span><span class="normal">Comparable</span><span class="symbol">[]</span><span class="normal"> a</span><span class="symbol">)</span><span class="normal"> </span><span class="cbracket">{</span>
<span class="normal">        StdRandom</span><span class="symbol">.</span><span class="function">shuffle</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">);</span>
<span class="normal">        </span><span class="function">sort</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">,</span><span class="normal"> </span><span class="number">0</span><span class="symbol">,</span><span class="normal"> a</span><span class="symbol">.</span><span class="normal">length </span><span class="symbol">-</span><span class="normal"> </span><span class="number">1</span><span class="symbol">);</span>
<span class="normal">        </span><span class="keyword">assert</span><span class="normal"> </span><span class="function">isSorted</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">);</span>
<span class="normal">    </span><span class="cbracket">}</span>

<span class="normal">    </span><span class="comment">// quicksort the subarray a[lo .. hi] using 3-way partitioning</span>
<span class="normal">    </span><span class="keyword">private</span><span class="normal"> </span><span class="keyword">static</span><span class="normal"> </span><span class="type">void</span><span class="normal"> </span><span class="function">sort</span><span class="symbol">(</span><span class="normal">Comparable</span><span class="symbol">[]</span><span class="normal"> a</span><span class="symbol">,</span><span class="normal"> </span><span class="type">int</span><span class="normal"> lo</span><span class="symbol">,</span><span class="normal"> </span><span class="type">int</span><span class="normal"> hi</span><span class="symbol">)</span><span class="normal"> </span><span class="cbracket">{</span><span class="normal"> </span>
<span class="normal">        </span><span class="keyword">if</span><span class="normal"> </span><span class="symbol">(</span><span class="normal">hi </span><span class="symbol">&lt;=</span><span class="normal"> lo</span><span class="symbol">)</span><span class="normal"> </span><span class="keyword">return</span><span class="symbol">;</span>
<span class="normal">        </span><span class="type">int</span><span class="normal"> lt </span><span class="symbol">=</span><span class="normal"> lo</span><span class="symbol">,</span><span class="normal"> gt </span><span class="symbol">=</span><span class="normal"> hi</span><span class="symbol">;</span>
<span class="normal">        </span><span class="usertype">Comparable</span><span class="normal"> v </span><span class="symbol">=</span><span class="normal"> a</span><span class="symbol">[</span><span class="normal">lo</span><span class="symbol">];</span>
<span class="normal">        </span><span class="type">int</span><span class="normal"> i </span><span class="symbol">=</span><span class="normal"> lo </span><span class="symbol">+</span><span class="normal"> </span><span class="number">1</span><span class="symbol">;</span>
<span class="normal">        </span><span class="keyword">while</span><span class="normal"> </span><span class="symbol">(</span><span class="normal">i </span><span class="symbol">&lt;=</span><span class="normal"> gt</span><span class="symbol">)</span><span class="normal"> </span><span class="cbracket">{</span>
<span class="normal">            </span><span class="type">int</span><span class="normal"> cmp </span><span class="symbol">=</span><span class="normal"> a</span><span class="symbol">[</span><span class="normal">i</span><span class="symbol">].</span><span class="function">compareTo</span><span class="symbol">(</span><span class="normal">v</span><span class="symbol">);</span>
<span class="normal">            </span><span class="keyword">if</span><span class="normal">      </span><span class="symbol">(</span><span class="normal">cmp </span><span class="symbol">&lt;</span><span class="normal"> </span><span class="number">0</span><span class="symbol">)</span><span class="normal"> </span><span class="function">exch</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">,</span><span class="normal"> lt</span><span class="symbol">++,</span><span class="normal"> i</span><span class="symbol">++);</span>
<span class="normal">            </span><span class="keyword">else</span><span class="normal"> </span><span class="keyword">if</span><span class="normal"> </span><span class="symbol">(</span><span class="normal">cmp </span><span class="symbol">&gt;</span><span class="normal"> </span><span class="number">0</span><span class="symbol">)</span><span class="normal"> </span><span class="function">exch</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">,</span><span class="normal"> i</span><span class="symbol">,</span><span class="normal"> gt</span><span class="symbol">--);</span>
<span class="normal">            </span><span class="keyword">else</span><span class="normal">              i</span><span class="symbol">++;</span>
<span class="normal">        </span><span class="cbracket">}</span>

<span class="normal">        </span><span class="comment">// a[lo..lt-1] &lt; v = a[lt..gt] &lt; a[gt+1..hi]. </span>
<span class="normal">        </span><span class="function">sort</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">,</span><span class="normal"> lo</span><span class="symbol">,</span><span class="normal"> lt</span><span class="symbol">-</span><span class="number">1</span><span class="symbol">);</span>
<span class="normal">        </span><span class="function">sort</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">,</span><span class="normal"> gt</span><span class="symbol">+</span><span class="number">1</span><span class="symbol">,</span><span class="normal"> hi</span><span class="symbol">);</span>
<span class="normal">        </span><span class="keyword">assert</span><span class="normal"> </span><span class="function">isSorted</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">,</span><span class="normal"> lo</span><span class="symbol">,</span><span class="normal"> hi</span><span class="symbol">);</span>
<span class="normal">    </span><span class="cbracket">}</span>



<span class="normal">   </span><span class="comment">/***************************************************************************</span>
<span class="comment">    *  Helper sorting functions.</span>
<span class="comment">    ***************************************************************************/</span>
<span class="normal">    </span>
<span class="normal">    </span><span class="comment">// is v &lt; w ?</span>
<span class="normal">    </span><span class="keyword">private</span><span class="normal"> </span><span class="keyword">static</span><span class="normal"> </span><span class="type">boolean</span><span class="normal"> </span><span class="function">less</span><span class="symbol">(</span><span class="usertype">Comparable</span><span class="normal"> v</span><span class="symbol">,</span><span class="normal"> </span><span class="usertype">Comparable</span><span class="normal"> w</span><span class="symbol">)</span><span class="normal"> </span><span class="cbracket">{</span>
<span class="normal">        </span><span class="keyword">return</span><span class="normal"> v</span><span class="symbol">.</span><span class="function">compareTo</span><span class="symbol">(</span><span class="normal">w</span><span class="symbol">)</span><span class="normal"> </span><span class="symbol">&lt;</span><span class="normal"> </span><span class="number">0</span><span class="symbol">;</span>
<span class="normal">    </span><span class="cbracket">}</span>
<span class="normal">        </span>
<span class="normal">    </span><span class="comment">// exchange a[i] and a[j]</span>
<span class="normal">    </span><span class="keyword">private</span><span class="normal"> </span><span class="keyword">static</span><span class="normal"> </span><span class="type">void</span><span class="normal"> </span><span class="function">exch</span><span class="symbol">(</span><span class="normal">Object</span><span class="symbol">[]</span><span class="normal"> a</span><span class="symbol">,</span><span class="normal"> </span><span class="type">int</span><span class="normal"> i</span><span class="symbol">,</span><span class="normal"> </span><span class="type">int</span><span class="normal"> j</span><span class="symbol">)</span><span class="normal"> </span><span class="cbracket">{</span>
<span class="normal">        </span><span class="usertype">Object</span><span class="normal"> swap </span><span class="symbol">=</span><span class="normal"> a</span><span class="symbol">[</span><span class="normal">i</span><span class="symbol">];</span>
<span class="normal">        a</span><span class="symbol">[</span><span class="normal">i</span><span class="symbol">]</span><span class="normal"> </span><span class="symbol">=</span><span class="normal"> a</span><span class="symbol">[</span><span class="normal">j</span><span class="symbol">];</span>
<span class="normal">        a</span><span class="symbol">[</span><span class="normal">j</span><span class="symbol">]</span><span class="normal"> </span><span class="symbol">=</span><span class="normal"> swap</span><span class="symbol">;</span>
<span class="normal">    </span><span class="cbracket">}</span>


<span class="normal">   </span><span class="comment">/***************************************************************************</span>
<span class="comment">    *  Check if array is sorted - useful for debugging.</span>
<span class="comment">    ***************************************************************************/</span>
<span class="normal">    </span><span class="keyword">private</span><span class="normal"> </span><span class="keyword">static</span><span class="normal"> </span><span class="type">boolean</span><span class="normal"> </span><span class="function">isSorted</span><span class="symbol">(</span><span class="normal">Comparable</span><span class="symbol">[]</span><span class="normal"> a</span><span class="symbol">)</span><span class="normal"> </span><span class="cbracket">{</span>
<span class="normal">        </span><span class="keyword">return</span><span class="normal"> </span><span class="function">isSorted</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">,</span><span class="normal"> </span><span class="number">0</span><span class="symbol">,</span><span class="normal"> a</span><span class="symbol">.</span><span class="normal">length </span><span class="symbol">-</span><span class="normal"> </span><span class="number">1</span><span class="symbol">);</span>
<span class="normal">    </span><span class="cbracket">}</span>

<span class="normal">    </span><span class="keyword">private</span><span class="normal"> </span><span class="keyword">static</span><span class="normal"> </span><span class="type">boolean</span><span class="normal"> </span><span class="function">isSorted</span><span class="symbol">(</span><span class="normal">Comparable</span><span class="symbol">[]</span><span class="normal"> a</span><span class="symbol">,</span><span class="normal"> </span><span class="type">int</span><span class="normal"> lo</span><span class="symbol">,</span><span class="normal"> </span><span class="type">int</span><span class="normal"> hi</span><span class="symbol">)</span><span class="normal"> </span><span class="cbracket">{</span>
<span class="normal">        </span><span class="keyword">for</span><span class="normal"> </span><span class="symbol">(</span><span class="type">int</span><span class="normal"> i </span><span class="symbol">=</span><span class="normal"> lo </span><span class="symbol">+</span><span class="normal"> </span><span class="number">1</span><span class="symbol">;</span><span class="normal"> i </span><span class="symbol">&lt;=</span><span class="normal"> hi</span><span class="symbol">;</span><span class="normal"> i</span><span class="symbol">++)</span>
<span class="normal">            </span><span class="keyword">if</span><span class="normal"> </span><span class="symbol">(</span><span class="function">less</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">[</span><span class="normal">i</span><span class="symbol">],</span><span class="normal"> a</span><span class="symbol">[</span><span class="normal">i</span><span class="symbol">-</span><span class="number">1</span><span class="symbol">]))</span><span class="normal"> </span><span class="keyword">return</span><span class="normal"> </span><span class="keyword">false</span><span class="symbol">;</span>
<span class="normal">        </span><span class="keyword">return</span><span class="normal"> </span><span class="keyword">true</span><span class="symbol">;</span>
<span class="normal">    </span><span class="cbracket">}</span>



<span class="normal">    </span><span class="comment">// print array to standard output</span>
<span class="normal">    </span><span class="keyword">private</span><span class="normal"> </span><span class="keyword">static</span><span class="normal"> </span><span class="type">void</span><span class="normal"> </span><span class="function">show</span><span class="symbol">(</span><span class="normal">Comparable</span><span class="symbol">[]</span><span class="normal"> a</span><span class="symbol">)</span><span class="normal"> </span><span class="cbracket">{</span>
<span class="normal">        </span><span class="keyword">for</span><span class="normal"> </span><span class="symbol">(</span><span class="type">int</span><span class="normal"> i </span><span class="symbol">=</span><span class="normal"> </span><span class="number">0</span><span class="symbol">;</span><span class="normal"> i </span><span class="symbol">&lt;</span><span class="normal"> a</span><span class="symbol">.</span><span class="normal">length</span><span class="symbol">;</span><span class="normal"> i</span><span class="symbol">++)</span><span class="normal"> </span><span class="cbracket">{</span>
<span class="normal">            StdOut</span><span class="symbol">.</span><span class="function">println</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">[</span><span class="normal">i</span><span class="symbol">]);</span>
<span class="normal">        </span><span class="cbracket">}</span>
<span class="normal">    </span><span class="cbracket">}</span>

<span class="normal">    </span><span class="comment">/**</span>
<span class="comment">     * Reads in a sequence of strings from standard input; 3-way</span>
<span class="comment">     * quicksorts them; and prints them to standard output in ascending order. </span>
<span class="comment">     *</span>
<span class="comment">     * </span><span class="type">@param</span><span class="comment"> args the command-line arguments</span>
<span class="comment">     */</span>
<span class="normal">    </span><span class="keyword">public</span><span class="normal"> </span><span class="keyword">static</span><span class="normal"> </span><span class="type">void</span><span class="normal"> </span><span class="function">main</span><span class="symbol">(</span><span class="normal">String</span><span class="symbol">[]</span><span class="normal"> args</span><span class="symbol">)</span><span class="normal"> </span><span class="cbracket">{</span>
<span class="normal">        String</span><span class="symbol">[]</span><span class="normal"> a </span><span class="symbol">=</span><span class="normal"> StdIn</span><span class="symbol">.</span><span class="function">readAllStrings</span><span class="symbol">();</span>
<span class="normal">        Quick3way</span><span class="symbol">.</span><span class="function">sort</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">);</span>
<span class="normal">        </span><span class="function">show</span><span class="symbol">(</span><span class="normal">a</span><span class="symbol">);</span>
<span class="normal">    </span><span class="cbracket">}</span>

<span class="cbracket">}</span>

<span class="comment">/******************************************************************************</span>
<span class="comment"> *  Copyright 2002-2018, Robert Sedgewick and Kevin Wayne.</span>
<span class="comment"> *</span>
<span class="comment"> *  This file is part of algs4.jar, which accompanies the textbook</span>
<span class="comment"> *</span>
<span class="comment"> *      Algorithms, 4th edition by Robert Sedgewick and Kevin Wayne,</span>
<span class="comment"> *      Addison-Wesley Professional, 2011, ISBN 0-321-57351-X.</span>
<span class="comment"> *      </span><span class="url">http://algs4.cs.princeton.edu</span>
<span class="comment"> *</span>
<span class="comment"> *</span>
<span class="comment"> *  algs4.jar is free software: you can redistribute it and/or modify</span>
<span class="comment"> *  it under the terms of the GNU General Public License as published by</span>
<span class="comment"> *  the Free Software Foundation, either version 3 of the License, or</span>
<span class="comment"> *  (at your option) any later version.</span>
<span class="comment"> *</span>
<span class="comment"> *  algs4.jar is distributed in the hope that it will be useful,</span>
<span class="comment"> *  but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="comment"> *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="comment"> *  GNU General Public License for more details.</span>
<span class="comment"> *</span>
<span class="comment"> *  You should have received a copy of the GNU General Public License</span>
<span class="comment"> *  along with algs4.jar.  If not, see </span><span class="url">http://www.gnu.org/licenses.</span>
<span class="comment"> ******************************************************************************/</span>
</tt></pre>

<script type="text/javascript">
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>
<script type="text/javascript">
try {
var pageTracker = _gat._getTracker("UA-10811519-2");
pageTracker._trackPageview();
} catch(err) {}</script>

</body>

<p><br><address><small>
Last updated: Sun Sep  2 16:49:18 EDT 2018.
</small></address>

</html>
