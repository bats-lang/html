#include "share/atspre_staload.hats"
#use array as A
#use wasm.bats-packages.dev/html as H

(* Every record kind named *)
fn record_name {lb:agz}{n:pos} (buf: !$A.borrow(byte, lb, n)): string =
  case+ $H.opcode(buf, 0) of
  | $H.ElementOpen() => "open"
  | $H.ElementClose() => "close"
  | $H.Text() => "text"
  | $H.NotARecord() => "end"
