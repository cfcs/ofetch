## ofetch

This is a HTTP client relying solely on the OCaml standard library.

The goal is to provide a dependency-free, safe utility to bootstrap
[opam](https://github.com/ocaml/opam), from where you can download a more
full-fletched HTTP client utilizing a more advanced parsing strategy.

It provides a [library](./ofetch.ml) and a
[command-line tool](./ofetch_cli.ml) (the `ofetch` binary).

The library supports concurrent fetching of multiple files.

### Limitations

- [ ] The return code does not reflect the status of the download operation.
- [ ] The HTTP response code is currently ignored.
- [ ] Retrying / resuming is not implemented.
- [ ] Parallelized fetching of the same file is not implemented.
  - [ ] Pipelined (using `Connection: keep-alive`) fetching is not implemented.
- [ ] Concurrent fetching of multiple files is not implemented in the CLI tool.
- [ ] Writes to disk are blocking.
- [ ] HTTPS is not implemented (to avoid depending on C-based TLS
      implementations).      
      A switch for building with [tls](https://github.com/mirleft/tls) support
      is on the drawing board.

### Usage

```shell
./_build/ofetch
Usage: ./_build/ofetch [-v] <LOCAL-FILENAME> <URL>

./_build/ofetch -v my.new.file http://example.com/cool.file.tar.xz
```

### Building

Building the `ofetch` binary:
```shell
# same as 'make native', built the ELF binary:
make
```

Running the test suite:
```shell
# note that 'make test' requires 'ocamlfind', and the
# ocaml libraries 'alcotest' and 'qcheck':
make test
# build the ofetch binary and try downloading various files from the internet,
# running wget in parallel and comparing the downloaded files:
make network-test
```

For debugging there is also
```shell
# build a bytecode executable for use with 'ocamldebug':
make debug
```

### Contributing

- Samples that trigger parsing/state machine errors are greatly appreciated.
- Extending the test suite with more coverage would also be great!
- CI would be amazing, too!
