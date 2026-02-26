# html

HTML parsing for [Bats](https://github.com/bats-lang) WASM applications.

## Features

- Parse HTML strings via the browser's DOM parser
- Iterate parsed tokens: element open, element close, text
- Get tag names and text content from parsed results

## Usage

```bats
#use wasm.bats-packages.dev/html as H

val () = $H.parse_html(src_bv, src_len)
val kind = $H.get_result(idx, buf, buf_len)
```

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Target

WASM only (`#target wasm`).
