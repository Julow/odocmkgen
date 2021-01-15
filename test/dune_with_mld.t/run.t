A basic test for working with Dune's _build/install.

  $ dune build -p test

Prepare packages:

  $ dune exec -- odocmkgen prepare-packages -o prep test
  Copy '$TESTCASE_ROOT/_build/install/default/lib/test/test.cmi' -> 'prep/test/test.cmi'
  Copy '$TESTCASE_ROOT/_build/install/default/lib/test/test.cmt' -> 'prep/test/test.cmt'
  Copy '$TESTCASE_ROOT/_build/install/default/lib/test/test.cmti' -> 'prep/test/test.cmti'
  Copy '$TESTCASE_ROOT/_build/install/default/doc/test/odoc-pages/test.mld' -> 'prep/test/test.mld'

Build:

  $ odocmkgen -- prep/* > Makefile

  $ make html
  odocmkgen gen prep/test
  Warning, couldn't find dep CamlinternalFormatBasics of file prep/test/test.cmti
  Warning, couldn't find dep Stdlib of file prep/test/test.cmti
  'odoc' 'compile' '--package' 'test' 'prep/test/test.cmti' '-o' 'odocs/test/test.odoc'
  'odoc' 'link' 'odocs/test/test.odoc' '-o' 'odocls/test/test.odocl' '-I' 'odocs/test/'
  'odocmkgen' 'generate' '--package' 'test'
  dir=test file=Test
  odoc support-files --output-dir html
  odoc html-generate odocls/test/test.odocl --output-dir html

  $ jq_scan_references() { jq -c '.. | .["`Reference"]? | select(.) | .[0]'; }

Doesn't resolve but should:

  $ odoc_print odocls/test/page-test.odocl | jq_scan_references
  odoc_print: PATH argument: no `odocls/test/page-test.odocl' file or directory
  Usage: odoc_print [OPTION]... PATH
  Try `odoc_print --help' for more information.
