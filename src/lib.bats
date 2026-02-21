(* html -- HTML parsing via JS DOMParser with cursor-based SAX reader *)

#include "share/atspre_staload.hats"

#use array as A
#use wasm.bats-packages.dev/bridge as B

#pub fun parse_html
  {lb:agz}{n:pos}
  (html: !$A.borrow(byte, lb, n), len: int n): int

#pub fun get_result
  {n:pos | n <= 1048576}
  (len: int n): [l:agz] $A.arr(byte, l, n)

#pub stadef ELEMENT_OPEN = 1
#pub stadef ELEMENT_CLOSE = 2
#pub stadef TEXT = 3

#pub fun opcode
  {lb:agz}{n:pos}{p:nat | p < n}
  (buf: !$A.borrow(byte, lb, n), pos: int p): int

#pub fun element_open
  {lb:agz}{n:pos}{p:nat | p < n}
  (buf: !$A.borrow(byte, lb, n), pos: int p, len: int n)
  : @(int, int, int, int)

#pub fun read_attr
  {lb:agz}{n:pos}{p:nat | p < n}
  (buf: !$A.borrow(byte, lb, n), pos: int p, len: int n)
  : @(int, int, int, int, int)

#pub fun read_text
  {lb:agz}{n:pos}{p:nat | p < n}
  (buf: !$A.borrow(byte, lb, n), pos: int p, len: int n)
  : @(int, int, int)

implement parse_html{lb}{n}(html, len) =
  $B.xml_parse(html, len)

implement get_result{n}(len) = $B.xml_result(len)

implement opcode{lb}{n}{p}(buf, pos) =
  byte2int0($A.read<byte>(buf, pos))

fn _peek{lb:agz}{n:pos}
  (buf: !$A.borrow(byte, lb, n), off: int, len: int n): int = let
  val off1 = g1ofg0(off)
in
  if off1 >= 0 then
    if off1 < len then
      byte2int0($A.read<byte>(buf, off1))
    else ~1
  else ~1
end

implement element_open{lb}{n}{p}(buf, pos, len) = let
  val p0 : int = g0ofg1(pos)
  val tag_len = _peek(buf, p0 + 1, len)
in
  if tag_len >= 0 then let
    val tag_off = p0 + 2
    val after_tag = tag_off + tag_len
    val attr_count = _peek(buf, after_tag, len)
  in
    if attr_count >= 0 then
      @(tag_off, tag_len, attr_count, after_tag + 1)
    else @(0, 0, 0, ~1)
  end
  else @(0, 0, 0, ~1)
end

implement read_attr{lb}{n}{p}(buf, pos, len) = let
  val p0 : int = g0ofg1(pos)
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
      in @(name_off, name_len, val_off, val_len, val_off + val_len) end
      else @(0, 0, 0, 0, ~1)
    else @(0, 0, 0, 0, ~1)
  end
  else @(0, 0, 0, 0, ~1)
end

implement read_text{lb}{n}{p}(buf, pos, len) = let
  val p0 : int = g0ofg1(pos)
  val lo = _peek(buf, p0 + 1, len)
  val hi = _peek(buf, p0 + 2, len)
in
  if lo >= 0 then
    if hi >= 0 then let
      val text_len = lo + hi * 256
      val text_off = p0 + 3
    in @(text_off, text_len, text_off + text_len) end
    else @(0, 0, ~1)
  else @(0, 0, ~1)
end
