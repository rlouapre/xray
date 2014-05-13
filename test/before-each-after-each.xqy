xquery version "1.0-ml";

module namespace test = "http://github.com/robwhitby/xray/test";
import module namespace assert = "http://github.com/robwhitby/xray/assertions" at "/xray/src/assertions.xqy";


(:
  optional setup function evaluated first
  add any test docs used by the tests in this module
:)

declare private variable $test-values := ();

declare %test:before-each function before-each()
{
  xdmp:set($test-values, ("hello", xdmp:transaction()))
};

declare %test:after-each function after-each()
{
  ()
};

declare %test:case function check-sequence-count() {
  assert:equal(fn:count($test-values), 2)
};

declare %test:case function check-first-value() {
  assert:equal($test-values[1], "hello")
};

declare %test:case function check-transaction-id() {
  assert:equal($test-values[2], xdmp:transaction())
};

