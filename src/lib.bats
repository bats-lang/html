(* html -- HTML parsing via JS DOMParser with cursor-based SAX reader *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use result as R

(* Parsing goes through the browser's DOMParser (bridge's xml host
   functions), so it exists only in WASM builds. The cursor decoders
   below are pure and available on every target. *)
#target wasm begin
staload XML = "wasm.bats-packages.dev/bridge/src/xml.sats"

#pub fun parse_html
  {lb:agz}{n:pos}
  (html: !$A.borrow(byte, lb, n), len: int n): $R.result(int, int)

#pub fun get_result
  {n:pos | n <= 1048576}
  (len: int n): [l:agz] $A.arr(byte, l, n)
end (* #target wasm *)

#pub stadef ELEMENT_OPEN = 1
#pub stadef ELEMENT_CLOSE = 2
#pub stadef TEXT = 3

#pub fun opcode
  {lb:agz}{n:pos}{p:nat | p < n}
  (buf: !$A.borrow(byte, lb, n), pos: int p): int

#pub fun element_open
  {lb:agz}{n:pos}{p:nat | p < n}
  (buf: !$A.borrow(byte, lb, n), pos: int p, len: int n)
  : $R.option(@(int, int, int, int))

#pub fun read_attr
  {lb:agz}{n:pos}{p:nat | p < n}
  (buf: !$A.borrow(byte, lb, n), pos: int p, len: int n)
  : $R.option(@(int, int, int, int, int))

#pub fun read_text
  {lb:agz}{n:pos}{p:nat | p < n}
  (buf: !$A.borrow(byte, lb, n), pos: int p, len: int n)
  : $R.option(@(int, int, int))

#target wasm begin
implement parse_html{lb}{n}(html, len) = let
  val byte_length = $XML.xml_parse(html, len)
in
  if byte_length > 0 then $R.ok(byte_length)
  else $R.err(0)
end

implement get_result{n}(len) = $XML.xml_result(len)
end (* #target wasm *)

implement opcode{lb}{n}{p}(buf, pos) =
  byte2int0($A.read<byte>(buf, pos))

(* Byte at off, or ~1 when off is outside the buffer. The two refining
   comparisons prove the read; the byte comes back as a bounded int, so
   offsets computed from it stay indexed too. *)
fn _peek{lb:agz}{n:pos}{o:int}
  (buf: !$A.borrow(byte, lb, n), off: int o, len: int n): [v:int | v >= ~1; v < 256] int v =
  if off >= 0 then
    if off < len then $AR.low_byte(byte2int0($A.read<byte>(buf, off)))
    else ~1
  else ~1

implement element_open{lb}{n}{p}(buf, pos, len) = let
  val p0 = pos
  val tag_len = _peek(buf, p0 + 1, len)
in
  if tag_len >= 0 then let
    val tag_off = p0 + 2
    val after_tag = tag_off + tag_len
    val attr_count = _peek(buf, after_tag, len)
  in
    if attr_count >= 0 then
      $R.some(@(tag_off, tag_len, attr_count, after_tag + 1))
    else $R.none()
  end
  else $R.none()
end

implement read_attr{lb}{n}{p}(buf, pos, len) = let
  val p0 = pos
  val name_len = _peek(buf, p0, len)
in
  if name_len >= 0 then let
    val name_off = p0 + 1
    val after_name = name_off + name_len
    val val_lo = _peek(buf, after_name, len)
    val val_hi = _peek(buf, after_name + 1, len)
  in
    if val_lo >= 0 then
      if val_hi >= 0 then let
        val val_len = val_lo + val_hi * 256
        val val_off = after_name + 2
      in $R.some(@(name_off, name_len, val_off, val_len, val_off + val_len)) end
      else $R.none()
    else $R.none()
  end
  else $R.none()
end

implement read_text{lb}{n}{p}(buf, pos, len) = let
  val p0 = pos
  val lo = _peek(buf, p0 + 1, len)
  val hi = _peek(buf, p0 + 2, len)
in
  if lo >= 0 then
    if hi >= 0 then let
      val text_len = lo + hi * 256
      val text_off = p0 + 3
    in $R.some(@(text_off, text_len, text_off + text_len)) end
    else $R.none()
  else $R.none()
end
