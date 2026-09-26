#include "share/atspre_staload.hats"
#use array as A
#use result as R
#use str as S
#use wasm.bats-packages.dev/html as H

(* Decodes a hand-built opcode buffer:
     [1, 2, 'h', 'i', 1]           element_open: tag "hi", 1 attr
     [1, 'k', 2, 0, 'v', 'w']      read_attr: name "k", value "vw"
     [3, 2, 0, 'x', 'y']           read_text at 11: "xy"
   and truncated variants, which must decode to none rather than read
   past the end. Exits 1 on any mismatch. *)
fn check (name: string, ok: bool): bool = let
  val () = (if ok then () else println! ("FAIL ", name))
in ok end

implement main0 () = let
  var s = @[char][16]('\001', '\002', 'h', 'i', '\001', '\001', 'k', '\002', '\000', 'v', 'w', '\003', '\002', '\000', 'x', 'y')
  val @(f, b) = $A.freeze<byte>($S.from_char_array(s, 16))
  val r1 = check("element_open", (case+ $H.element_open(b, 0, 16) of
    | ~$R.some(@(to, tl, ac, nx)) => to = 2 && tl = 2 && ac = 1 && nx = 5
    | ~$R.none() => false))
  val r2 = check("read_attr", (case+ $H.read_attr(b, 5, 16) of
    | ~$R.some(@(no, nl, vo, vl, nx)) => no = 6 && nl = 1 && vo = 9 && vl = 2 && nx = 11
    | ~$R.none() => false))
  val r3 = check("read_text", (case+ $H.read_text(b, 11, 16) of
    | ~$R.some(@(to, tl, nx)) => to = 14 && tl = 2 && nx = 16
    | ~$R.none() => false))
  val r4 = check("read_text truncated", (case+ $H.read_text(b, 15, 16) of
    | ~$R.some(_) => false | ~$R.none() => true))
  val r5 = check("element_open past attr count", (case+ $H.element_open(b, 14, 16) of
    | ~$R.some(_) => false | ~$R.none() => true))
  val () = $A.drop<byte>(f, b)
  val () = $A.free<byte>($A.thaw<byte>(f))
in
  if r1 && r2 && r3 && r4 && r5 then println! ("decode: all cases pass")
  else exit_void(1)
end
