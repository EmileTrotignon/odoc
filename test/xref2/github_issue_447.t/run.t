This test checks for handling of type substitutions.
Specifically, there was an issue where the code in this test
caused an exception during compilation. The absence of the
exception shows this working correctly.

  $ ocamlc -c -bin-annot a.mli
  $ odoc compile --warn-error -I . a.cmti
  $ odoc link a.odoc
  $ odoc html-generate --output-dir . a.odocl
  $ cat A/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>A (A)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/><script src="../highlight.pack.js"></script><script>hljs.initHighlightingOnLoad();</script></head><body class="odoc"><header class="odoc-preamble"><h1>Module <code><span>A</span></code></h1></header><div class="odoc-content"><div class="odoc-spec"><div class="spec module anchored" id="module-M"><a href="#module-M" class="anchor"></a><code><span><span class="keyword">module</span> <a href="M/index.html">M</a></span><span> : <span class="keyword">sig</span> ... <span class="keyword">end</span></span></code></div></div><p><a href="M/index.html#type-t.Foo"><code>M.t.Foo</code></a> <a href="M/index.html#type-t.Foo"><code>M.t.Foo</code></a></p></div></body></html>
