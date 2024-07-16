// Unmodified source here: https://gitlab.com/antora/antora-ui-default/-/blob/master/src/js/vendor/highlight.bundle.js

// In order to actually add a support for a certain language we need to repack the `highlight.js` file.
// This file only specifies supported languages.
// To get a `highlight.js` file you need to clone https://gitlab.com/antora/antora-ui-default/-/tree/master,
// change src/js/vendor/highlight.bundle.js file contents with lines from this file, 
// and then bundle a new `highlight.js` using instructions from README.
// Then just copy `highlight.js` from public/_/js/vendor directory here.

;(function () {
    'use strict'
  
    var hljs = require('highlight.js/lib/highlight')
    hljs.registerLanguage('asciidoc', require('highlight.js/lib/languages/asciidoc'))
    hljs.registerLanguage('bash', require('highlight.js/lib/languages/bash'))
    hljs.registerLanguage('clojure', require('highlight.js/lib/languages/clojure'))
    hljs.registerLanguage('cpp', require('highlight.js/lib/languages/cpp'))
    hljs.registerLanguage('cs', require('highlight.js/lib/languages/cs'))
    hljs.registerLanguage('css', require('highlight.js/lib/languages/css'))
    hljs.registerLanguage('diff', require('highlight.js/lib/languages/diff'))
    hljs.registerLanguage('dockerfile', require('highlight.js/lib/languages/dockerfile'))
    hljs.registerLanguage('elixir', require('highlight.js/lib/languages/elixir'))
    hljs.registerLanguage('go', require('highlight.js/lib/languages/go'))
    hljs.registerLanguage('groovy', require('highlight.js/lib/languages/groovy'))
    hljs.registerLanguage('haskell', require('highlight.js/lib/languages/haskell'))
    hljs.registerLanguage('java', require('highlight.js/lib/languages/java'))
    hljs.registerLanguage('javascript', require('highlight.js/lib/languages/javascript'))
    hljs.registerLanguage('json', require('highlight.js/lib/languages/json'))
    hljs.registerLanguage('julia', require('highlight.js/lib/languages/julia'))
    hljs.registerLanguage('kotlin', require('highlight.js/lib/languages/kotlin'))
    hljs.registerLanguage('lua', require('highlight.js/lib/languages/lua'))
    hljs.registerLanguage('markdown', require('highlight.js/lib/languages/markdown'))
    hljs.registerLanguage('nix', require('highlight.js/lib/languages/nix'))
    hljs.registerLanguage('none', require('highlight.js/lib/languages/plaintext'))
    hljs.registerLanguage('objectivec', require('highlight.js/lib/languages/objectivec'))
    hljs.registerLanguage('perl', require('highlight.js/lib/languages/perl'))
    hljs.registerLanguage('php', require('highlight.js/lib/languages/php'))
    hljs.registerLanguage('properties', require('highlight.js/lib/languages/properties'))
    hljs.registerLanguage('puppet', require('highlight.js/lib/languages/puppet'))
    hljs.registerLanguage('python', require('highlight.js/lib/languages/python'))
    hljs.registerLanguage('ruby', require('highlight.js/lib/languages/ruby'))
    hljs.registerLanguage('rust', require('highlight.js/lib/languages/rust'))
    hljs.registerLanguage('scala', require('highlight.js/lib/languages/scala'))
    hljs.registerLanguage('shell', require('highlight.js/lib/languages/shell'))
    hljs.registerLanguage('sql', require('highlight.js/lib/languages/sql'))
    hljs.registerLanguage('swift', require('highlight.js/lib/languages/swift'))
    hljs.registerLanguage('xml', require('highlight.js/lib/languages/xml'))
    hljs.registerLanguage('yaml', require('highlight.js/lib/languages/yaml'))

    hljs.registerLanguage('verilog', require('highlight.js/lib/languages/verilog'))
    hljs.registerLanguage('makefile', require('highlight.js/lib/languages/makefile'))

    ;[].slice.call(document.querySelectorAll('pre code.hljs[data-lang]')).forEach(function (node) {
      hljs.highlightBlock(node)
    })
  })()
  