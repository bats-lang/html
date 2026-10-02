#include "share/atspre_staload.hats"
#use array as A
#use arith as AR
#use str as S
#use wasm.bats-packages.dev/html as H

(* Prints what sanitize keeps of hand-built raw streams, as HTML: the
   stream is read with opcode, element_open, read_attr and read_text *)

(* Prints buf[at, at + k) *)
fun print_span {lb:agz}{n:pos}{at,k:nat | at + k <= n}{i:nat | i <= k} .<k - i>.
  (buf: !$A.borrow(byte, lb, n), at: int at, k: int k, i: int i): void =
  if i >= k then ()
  else let
    val () = print_char(int2char0(byte2int0($A.read<byte>(buf, at + i))))
  in print_span(buf, at, k, i + 1) end

(* The byte at i, as a bounded int *)
fn byte_at {lb:agz}{n:pos}{i:nat | i < n} (buf: !$A.borrow(byte, lb, n), i: int i): [v:nat | v < 256] int v =
  $AR.low_byte(byte2int0($A.read<byte>(buf, i)))

(* Prints count attributes from buf[i]; where they end, or n when the
   stream is cut *)
fun print_attributes {lb:agz}{n:pos}{i:nat | i <= n}{count:nat} .<count>.
  (buf: !$A.borrow(byte, lb, n), n: int n, i: int i, count: int count): [j:nat | j <= n; j >= i] int j =
  if count <= 0 then i
  else if i >= n then n
  else let
    val name_len = byte_at(buf, i)
  in
    if i + 1 + name_len + 2 > n then n
    else let
      val value_len = byte_at(buf, i + 1 + name_len) + 256 * byte_at(buf, i + 2 + name_len)
      val value_at = i + 3 + name_len
    in
      if value_at + value_len > n then n
      else let
        val () = print_string(" ")
        val () = print_span(buf, i + 1, name_len, 0)
        val () = print_string("=")
        val () = print_span(buf, value_at, value_len, 0)
      in print_attributes(buf, n, value_at + value_len, count - 1) end
    end
  end

(* Prints the stream from buf[i], as HTML *)
fun print_stream {lb:agz}{n:pos}{i:nat | i <= n} .<n - i>.
  (buf: !$A.borrow(byte, lb, n), n: int n, i: int i): void =
  if i >= n then ()
  else
    case+ $H.opcode(buf, i) of
    | $H.ElementClose() => let
        val () = print_string("</>")
      in print_stream(buf, n, i + 1) end
    | $H.Text() =>
      (if i + 3 > n then print_string("[cut]")
       else let
         val text_len = byte_at(buf, i + 1) + 256 * byte_at(buf, i + 2)
       in
         if i + 3 + text_len > n then print_string("[cut]")
         else let
           val () = print_span(buf, i + 3, text_len, 0)
         in print_stream(buf, n, i + 3 + text_len) end
       end)
    | $H.ElementOpen() =>
      (if i + 2 > n then print_string("[cut]")
       else let
         val tag_len = byte_at(buf, i + 1)
       in
         if i + 3 + tag_len > n then print_string("[cut]")
         else let
           val () = print_string("<")
           val () = print_span(buf, i + 2, tag_len, 0)
           val next = print_attributes(buf, n, i + 3 + tag_len, byte_at(buf, i + 2 + tag_len))
           val () = print_string(">")
         in print_stream(buf, n, next) end
       end)
    | $H.NotARecord() => print_string("[?]")

fn show {n:pos | n <= 1048576} (name: string, raw: [l:agz] $A.arr(byte, l, n), n: int n): void = let
  val @(frozen, borrowed) = $A.freeze<byte>(raw)
  val () = print_string(name)
  val () = print_string(": ")
  val () = (case+ $H.sanitize(borrowed, n) of
    | ~$H.Parsed(kept, k) => let
        val @(kept_frozen, kept_borrowed) = $A.freeze<byte>(kept)
        val () = print_stream(kept_borrowed, k, 0)
        val () = $A.drop<byte>(kept_frozen, kept_borrowed)
      in $A.free<byte>($A.thaw<byte>(kept_frozen)) end
    | ~$H.NotParsed() => print_string("nothing"))
  val () = print_newline()
  val () = $A.drop<byte>(frozen, borrowed)
in $A.free<byte>($A.thaw<byte>(frozen)) end

implement main0 () = let
  var case0 = @[char][79]('\001', '\001', 'p', '\006', '\005', 'c', 'l', 'a', 's', 's', '\001', '\000', 'a', '\007', 'o', 'n', 'c', 'l', 'i', 'c', 'k', '\001', '\000', 'x', '\005', 's', 't', 'y', 'l', 'e', '\011', '\000', 'c', 'o', 'l', 'o', 'r', ':', 'r', 'e', 'd', '\006', 'd', 'a', 't', 'a', '-', 'n', '\001', '\000', '1', '\013', 'O', 'N', 'M', 'O', 'U', 'S', 'E', 'O', 'V', 'E', 'R', '\001', '\000', 'y', '\003', 'x', ':', 'y', '\001', '\000', 'z', '\003', '\002', '\000', 'h', 'i', '\002')
  val () = show("attributes", $S.from_char_array(case0, 79), 79)
  var case1 = @[char][33]('\001', '\003', 'd', 'i', 'v', '\000', '\001', '\006', 's', 'c', 'r', 'i', 'p', 't', '\000', '\003', '\010', '\000', 'a', 'l', 'e', 'r', 't', '\050', '1', ')', '\002', '\003', '\002', '\000', 'o', 'k', '\002')
  val () = show("script inside", $S.from_char_array(case1, 33), 33)
  var case2 = @[char][38]('\001', '\006', 'i', 'f', 'r', 'a', 'm', 'e', '\001', '\003', 's', 'r', 'c', '\001', '\000', 'x', '\001', '\001', 'p', '\000', '\003', '\002', '\000', 'i', 'n', '\002', '\002', '\001', '\001', 'b', '\000', '\003', '\003', '\000', 'o', 'u', 't', '\002')
  val () = show("dropped subtree", $S.from_char_array(case2, 38), 38)
  var case3 = @[char][39]('\001', '\006', 'S', 'C', 'R', 'I', 'P', 'T', '\000', '\003', '\001', '\000', 'x', '\002', '\001', '\004', 'f', 'o', 'r', 'm', '\000', '\001', '\005', 'i', 'n', 'p', 'u', 't', '\001', '\004', 'n', 'a', 'm', 'e', '\001', '\000', 'q', '\002', '\002')
  val () = show("all dropped", $S.from_char_array(case3, 39), 39)
  var case4 = @[char][17]('\001', '\001', 'i', '\000', '\003', '\005', '\000', 'w', 'h', 'o', 'l', 'e', '\002', '\001', '\003', 'e', 'm')
  val () = show("cut short", $S.from_char_array(case4, 17), 17)
in end
