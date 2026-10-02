# html

HTML parsing for [Bats](https://github.com/bats-lang) WASM applications.

## Features

- Parse HTML strings via the browser's DOM parser
- Iterate parsed tokens: element open, element close, text
- Get tag names and text content from parsed results

## Usage

```bats
#use wasm.bats-packages.dev/html as H

case+ $H.parse_html(src_bv, src_len) of
| ~$H.Parsed(document, k) => let
    val @(frozen, doc) = $A.freeze<byte>(document)
    val () = (case+ $H.opcode(doc, 0) of
      | $H.ElementOpen() => ... (* $H.element_open(doc, 0, k) *)
      | $H.ElementClose() => ...
      | $H.Text() => ...       (* $H.read_text(doc, 0, k) *)
      | $H.NotARecord() => ...)
    ...
  end
| ~$H.NotParsed() => ...
```

A record's kind is `record_kind` (`ElementOpen | ElementClose | Text |
NotARecord`), decoded from the stream's byte by `opcode`; match it with
`case+`.

## Sanitizing

The browser parses (bridge's `xml_parse` is DOMParser and a raw
serialisation of what it found). What is kept is decided here, in Bats:
`sanitize` drops the elements `script`, `iframe`, `object`, `embed`,
`form`, `input`, `link` and `meta` with everything in them, and the
attributes named `on…` (in any case), `style`, or anything but letters,
digits and hyphens. `parse_html` is the two together.

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Target

WASM only (`#target wasm`).
