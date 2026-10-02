#include "share/atspre_staload.hats"
#use array as A
#use wasm.bats-packages.dev/html as H

(* A record's kind is a choice, not the stream's byte *)
fn is_open {lb:agz}{n:pos} (buf: !$A.borrow(byte, lb, n)): bool =
  $H.opcode(buf, 0) = 1
