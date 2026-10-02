(* html -- HTML parsing via JS DOMParser with cursor-based SAX reader *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use result as R
#use wasm.bats-packages.dev/bridge as B

(* Parsing goes through the browser's DOMParser (bridge's xml host
   functions), so it exists only in WASM builds. The cursor decoders
   below are pure and available on every target. *)
(* The parsed document, SAX-encoded (read with opcode, element_open,
   read_attr and read_text), and its length; none when parsing produced
   nothing or more than 1 MiB *)
#pub datavtype parsed =
  | {l:agz}{k:pos | k <= 1048576} Parsed of ($A.arr(byte, l, k), int k)
  | NotParsed of ()

(* The document of a raw SAX stream (bridge's xml_parse: every element,
   attribute and text the browser's DOMParser found), without what could
   run code or load anything when it is shown:
   * the elements script, iframe, object, embed, form, input, link and
     meta, with everything in them;
   * attributes named on... (event handlers, in any case), style, and
     any whose name is not letters, digits and hyphens.
   A stream cut short ends the document there. NotParsed when nothing is
   left *)
#pub fun sanitize
  {lb:agz}{n:pos | n <= 1048576}
  (raw: !$A.borrow(byte, lb, n), len: int n): parsed

#target wasm begin
staload XML = "wasm.bats-packages.dev/bridge/src/xml.sats"
staload BD = "wasm.bats-packages.dev/bridge/src/decompress.sats"

(* The document of the HTML: parsed by the browser (xml_parse), then
   sanitized here *)
#pub fun parse_html
  {lb:agz}{n:pos}
  (html: !$A.borrow(byte, lb, n), len: int n): parsed
end (* #target wasm *)

(* What the record at a position of the stream is. The stream writes
   each as a byte, 1, 2 or 3, which opcode reads; any other byte is not
   a record (the stream ends there) *)
#pub datatype record_kind =
  | ElementOpen
  | ElementClose
  | Text
  | NotARecord

#pub fun opcode
  {lb:agz}{n:pos}{p:nat | p < n}
  (buf: !$A.borrow(byte, lb, n), pos: int p): record_kind

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
implement parse_html{lb}{n}(html, len) =
  case+ $XML.xml_parse(html, len) of
  | ~$R.none() => NotParsed()
  | ~$R.some(b) => let
      val k = $BD.blob_len(b)
    in
      if k <= 0 then let val () = $BD.blob_free(b) in NotParsed() end
      else if k > 1048576 then let val () = $BD.blob_free(b) in NotParsed() end
      else let
        val buf = $A.alloc<byte>(k)
        val () = $BD.blob_read(b, 0, buf, k)
        val () = $BD.blob_free(b)
        val @(frozen, raw) = $A.freeze<byte>(buf)
        val document = sanitize(raw, k)
        val () = $A.drop<byte>(frozen, raw)
        val () = $A.free<byte>($A.thaw<byte>(frozen))
      in document end
    end
end (* #target wasm *)

(* The kind a record's first byte names: the one place the stream's
   numbers for them are read *)
fn _record_kind (code: int): record_kind =
  if code = 1 then ElementOpen()
  else if code = 2 then ElementClose()
  else if code = 3 then Text()
  else NotARecord()

implement opcode{lb}{n}{p}(buf, pos) =
  _record_kind(byte2int0($A.read<byte>(buf, pos)))

(* The byte at off, which is inside the buffer, as a bounded int, so
   offsets computed from it stay indexed too *)
fn _byte{lb:agz}{n:pos}{o:nat | o < n}
  (buf: !$A.borrow(byte, lb, n), off: int o): [v:nat | v < 256] int v =
  $AR.low_byte(byte2int0($A.read<byte>(buf, off)))

implement element_open{lb}{n}{p}(buf, pos, len) =
  if pos + 1 >= len then $R.none()
  else let
    val tag_len = _byte(buf, pos + 1)
    val tag_off = pos + 2
    val after_tag = tag_off + tag_len
  in
    if after_tag >= len then $R.none()
    else $R.some(@(tag_off, tag_len, _byte(buf, after_tag), after_tag + 1))
  end

implement read_attr{lb}{n}{p}(buf, pos, len) = let
  val name_len = _byte(buf, pos)
  val name_off = pos + 1
  val after_name = name_off + name_len
in
  if after_name + 1 >= len then $R.none()
  else let
    val val_len = _byte(buf, after_name) + _byte(buf, after_name + 1) * 256
    val val_off = after_name + 2
  in $R.some(@(name_off, name_len, val_off, val_len, val_off + val_len)) end
end

implement read_text{lb}{n}{p}(buf, pos, len) =
  if pos + 2 >= len then $R.none()
  else let
    val text_len = _byte(buf, pos + 1) + _byte(buf, pos + 2) * 256
    val text_off = pos + 3
  in $R.some(@(text_off, text_len, text_off + text_len)) end

(* ============================================================
   Sanitizing: a walk over the raw stream, writing what is kept to out.
   Nothing is ever added, so what is written (o bytes) never passes what
   is read (i bytes): o <= i <= n, and every write is in range.
   ============================================================ *)

(* The byte at i, as a bounded int *)
fn _at {lb:agz}{n:pos}{i:nat | i < n} (raw: !$A.borrow(byte, lb, n), i: int i): [v:nat | v < 256] int v =
  $AR.low_byte(byte2int0($A.read<byte>(raw, i)))

(* c lower-cased, when it is an ASCII capital *)
fn _lower (c: int): int = if c >= 65 && c <= 90 then c + 32 else c

(* Whether raw[at, at + k) is word, in any case *)
fun _is_word {lb:agz}{n:pos}{at,k:nat | at + k <= n}{w:nat}{i:nat | i <= k} .<k - i>.
  (raw: !$A.borrow(byte, lb, n), at: int at, k: int k, word: string w, i: int i): bool =
  if k <> g1u2i(string1_length(word)) then false
  else if i >= k then true
  else if i >= g1u2i(string1_length(word)) then false
  else if _lower(byte2int0($A.read<byte>(raw, at + i))) <> char2int0(string_get_at(word, i)) then false
  else _is_word(raw, at, k, word, i + 1)

(* Whether an element named raw[at, at + k) is dropped with what is in
   it *)
fn _tag_dropped {lb:agz}{n:pos}{at,k:nat | at + k <= n}
  (raw: !$A.borrow(byte, lb, n), at: int at, k: int k): bool =
  _is_word(raw, at, k, "script", 0) || _is_word(raw, at, k, "iframe", 0)
  || _is_word(raw, at, k, "object", 0) || _is_word(raw, at, k, "embed", 0)
  || _is_word(raw, at, k, "form", 0) || _is_word(raw, at, k, "input", 0)
  || _is_word(raw, at, k, "link", 0) || _is_word(raw, at, k, "meta", 0)

(* Whether raw[at + i, at + k) is letters, digits and hyphens *)
fun _name_chars {lb:agz}{n:pos}{at,k:nat | at + k <= n}{i:nat | i <= k} .<k - i>.
  (raw: !$A.borrow(byte, lb, n), at: int at, k: int k, i: int i): bool =
  if i >= k then true
  else let
    val c = _lower(byte2int0($A.read<byte>(raw, at + i)))
  in
    if (c >= 97 && c <= 122) || (c >= 48 && c <= 57) || c = 45 then _name_chars(raw, at, k, i + 1)
    else false
  end

(* Whether an attribute named raw[at, at + k) is kept: a name of
   letters, digits and hyphens, not an event handler (on...) and not
   style *)
fn _attribute_kept {lb:agz}{n:pos}{at,k:nat | at + k <= n}
  (raw: !$A.borrow(byte, lb, n), at: int at, k: int k): bool =
  if k <= 0 then false
  else if not(_name_chars(raw, at, k, 0)) then false
  else if _is_word(raw, at, k, "style", 0) then false
  else if k >= 2 then
    not(_lower(byte2int0($A.read<byte>(raw, at))) = 111
        && _lower(byte2int0($A.read<byte>(raw, at + 1))) = 110)
  else true

(* out[o, o + k) := raw[i, i + k) *)
fun _copy {lb,lo:agz}{n:pos}{i,o,k:nat | i + k <= n; o + k <= n}{j:nat | j <= k} .<k - j>.
  (raw: !$A.borrow(byte, lb, n), out: !$A.arr(byte, lo, n), i: int i, o: int o, k: int k, j: int j): void =
  if j >= k then ()
  else let
    val () = $A.set<byte>(out, o + j, $A.read<byte>(raw, i + j))
  in _copy(raw, out, i, o, k, j + 1) end

(* Where a walk is: i bytes read, o written, and how many attributes of
   the element being written were kept; Cut when the stream ends inside
   a record *)
datavtype attributes_walked(n:int, start:int) =
  | {i,o:nat | o <= i; i <= n; i >= start}{kept:nat | kept < 256} Walked(n, start) of (int i, int o, int kept)
  | Cut(n, start) of ()

(* The attributes of an element: count of them left, from raw[i], each
   one kept written to out[o] when write is true (the element is kept) *)
fun _attributes {lb,lo:agz}{n:pos}{i,o:nat | o <= i; i <= n}{kept:nat | kept < 256}{left:nat} .<left>.
  (raw: !$A.borrow(byte, lb, n), out: !$A.arr(byte, lo, n), n: int n,
   i: int i, o: int o, left: int left, write: bool, kept: int kept): [s:int | s >= i] attributes_walked(n, s) =
  if left <= 0 then (Walked(i, o, kept): attributes_walked(n, i))
  else if i >= n then (Cut(): attributes_walked(n, i))
  else let
    val name_len = _at(raw, i)
    val name_at = i + 1
  in
    if name_at + name_len + 2 > n then (Cut(): attributes_walked(n, i))
    else let
      val value_len = _at(raw, name_at + name_len) + 256 * _at(raw, name_at + name_len + 1)
      val value_at = name_at + name_len + 2
    in
      if value_at + value_len > n then (Cut(): attributes_walked(n, i))
      else let
        val record_len = value_at + value_len - i
        val keep = (if write then (if _attribute_kept(raw, name_at, name_len) then kept < 255 else false) else false): bool
      in
        if keep then
          (if kept < 255 then let
             val () = _copy(raw, out, i, o, record_len, 0)
           in _attributes(raw, out, n, value_at + value_len, o + record_len, left - 1, write, kept + 1) end
           else _attributes(raw, out, n, value_at + value_len, o, left - 1, write, kept))
        else _attributes(raw, out, n, value_at + value_len, o, left - 1, write, kept)
      end
    end
  end

(* The walk: from raw[i], writing to out[o], inside skip dropped
   elements; the bytes written at its end *)
fun _walk {lb,lo:agz}{n:pos}{i,o:nat | o <= i; i <= n}{skip:nat} .<n - i, 1>.
  (raw: !$A.borrow(byte, lb, n), out: !$A.arr(byte, lo, n), n: int n,
   i: int i, o: int o, skip: int skip): [w:nat | w <= n] int w =
  if i >= n then o
  else
    case+ _record_kind(_at(raw, i)) of
    | ElementOpen() => _element(raw, out, n, i, o, skip)
    | ElementClose() =>
      (if skip > 0 then _walk(raw, out, n, i + 1, o, skip - 1)
       else let
         val () = $A.set<byte>(out, o, $A.read<byte>(raw, i))
       in _walk(raw, out, n, i + 1, o + 1, skip) end)
    | Text() =>
      (if i + 3 > n then o
       else let
         val text_len = _at(raw, i + 1) + 256 * _at(raw, i + 2)
       in
         if i + 3 + text_len > n then o
         else if skip > 0 then _walk(raw, out, n, i + 3 + text_len, o, skip)
         else let
           val () = _copy(raw, out, i, o, 3 + text_len, 0)
         in _walk(raw, out, n, i + 3 + text_len, o + 3 + text_len, skip) end
       end)
    | NotARecord() => o (* the stream ends here *)

(* An element opening at raw[i]: [1][tag length][tag][attribute count]
   [attributes]. Kept, its head and kept attributes are written with
   the count of those; dropped (or inside a dropped one), nothing is
   written until its close *)
and _element {lb,lo:agz}{n:pos}{i,o:nat | o <= i; i < n}{skip:nat} .<n - i, 0>.
  (raw: !$A.borrow(byte, lb, n), out: !$A.arr(byte, lo, n), n: int n,
   i: int i, o: int o, skip: int skip): [w:nat | w <= n] int w =
  if i + 2 > n then o
  else let
    val tag_len = _at(raw, i + 1)
    val tag_at = i + 2
  in
    if tag_at + tag_len + 1 > n then o
    else let
      val count = _at(raw, tag_at + tag_len)
      val head_len = 3 + tag_len
      val dropped = skip > 0 || _tag_dropped(raw, tag_at, tag_len)
    in
      if dropped then
        (case+ _attributes(raw, out, n, i + head_len, o, count, false, 0) of
         | ~Walked(next, _, _) => _walk(raw, out, n, next, o, skip + 1)
         | ~Cut() => o)
      else let
        val () = _copy(raw, out, i, o, head_len - 1, 0)
      in
        case+ _attributes(raw, out, n, i + head_len, o + head_len, count, true, 0) of
        | ~Walked(next, written, kept) => let
            val () = $A.set<byte>(out, o + head_len - 1, $A.int2byte(kept))
          in _walk(raw, out, n, next, written, skip) end
        | ~Cut() => o
      end
    end
  end

(* to[0, k) := from[0, k) *)
fun _copy_out {lf,lt:agz}{n,k:pos | k <= n}{j:nat | j <= k} .<k - j>.
  (from: !$A.arr(byte, lf, n), to: !$A.arr(byte, lt, k), k: int k, j: int j): void =
  if j >= k then ()
  else let
    val () = $A.set<byte>(to, j, $A.get<byte>(from, j))
  in _copy_out(from, to, k, j + 1) end

implement sanitize{lb}{n}(raw, len) = let
  val out = $A.alloc<byte>(len)
  val written = _walk(raw, out, len, 0, 0, 0)
in
  if written <= 0 then let val () = $A.free<byte>(out) in NotParsed() end
  else let
    val kept = $A.alloc<byte>(written)
    val () = _copy_out(out, kept, written, 0)
    val () = $A.free<byte>(out)
  in Parsed(kept, written) end
end
