#include "share/atspre_staload.hats"
#use array as A
#use wasm.bats-packages.dev/html as H

(* A match on a record's kind that leaves text out *)
fn record_name (kind: $H.record_kind): string =
  case+ kind of
  | $H.ElementOpen() => "open"
  | $H.ElementClose() => "close"
  | $H.NotARecord() => "end"
