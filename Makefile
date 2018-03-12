# shared flags for the ocaml compiler:
$(eval OFLAGS := -absname -unboxed-types -safe-string -verbose \
		 -w +1..999-57-42 -warn-error +a-26-27-4-33-42 )

# shared flags for c compilation (through the ocaml compiler):
$(eval CFLAGS := -ccopt -fPIE -compact )

ifeq "$(shell ocamlfind query tls 2>&- || true)" ""
$(eval OFETCH_WRAP_FILE := ofetch_unix)
$(eval OFETCH_WRAP_DEPS := ./ofetch_unix.mli)
$(eval OCOM := ocamlopt.opt unix.cmxa bigarray.cmxa )
else
$(eval OFETCH_WRAP_FILE := ofetch_tls_wrap)
$(eval OFETCH_WRAP_DEPS := ./ofetch_unix.mli ./ofetch_unix.ml ./ofetch_tls.ml)
$(eval OCOM := ocamlfind opt -package nocrypto.unix -package tls -linkpkg )
endif

cli: builddir lib ofetch_cli.ml
	@ cd _build/ && \
	set -x && \
	cp $(OFETCH_WRAP_FILE).ml ofetch_wrap.ml && \
	cp $(OFETCH_WRAP_FILE).mli ofetch_wrap.mli && \
	$(OCOM) $(OFLAGS) $(CFLAGS) \
		ofetch.cmx \
		$(OFETCH_WRAP_DEPS) ./ofetch_wrap.mli ./ofetch_wrap.ml \
		./ofetch_cli.ml -o ./ofetch && \
	{ { set +x ; } 2> /dev/null ; } && \
	strip ./ofetch && \
	ls -lh ./ofetch

builddir: *.ml
	@ mkdir -p _build/ && \
	  cp ./*.ml ./*.mli _build/

lib: builddir ofetch.ml
	@ cd _build/ && \
	ocamlopt.opt $(OFLAGS) $(CFLAGS) \
		-c -linkall \
		bigarray.cmxa ./ofetch.ml

test: builddir lib ofetch.ml tests.ml
	cd _build/ && \
	cp ../tests.ml . && \
	ocamlfind opt $(OFLAGS) $(CFLAGS) \
		-package alcotest -package qcheck -linkpkg \
		bigarray.cmxa ./ofetch.cmx ./tests.ml -o ./tests && \
		./tests && echo "tests ran, all good"

install: cli
	cd _build && cp ofetch $(PREFIX)/ofetch

debug: builddir ofetch.ml
	cd _build/ && \
	ocamlc.byte $(OFLAGS) \
		-g -bin-annot \
		unix.cma bigarray.cma ./ofetch.ml ./ofetch_cli.ml \
		-o ./ofetch.bc && \
	ocamldebug ./ofetch.bc

define compare_wget
	  #
	@ # first argument: testcase name
	@ # second argument: url to fetch
	@ echo "===== comparing against wget:" "$1" "$2"
	$(eval OFETCH:= "test.$1.ofetch")
	$(eval WGET := "test.$1.wget")
	@ cd _build/ && \
	rm -f $(OFETCH) $(WGET) && \
	set -x && \
	time wget --tries 1 -O $(WGET) "$2" && \
	time ./ofetch -v $(OFETCH) "$2" ; \
	{ { set +x ; } 2> /dev/null ; } && \
	{ \
	  { sha1sum "$(OFETCH)" > "$(OFETCH).sha1" && \
	    sha1sum "$(WGET)" > "$(WGET).sha1" && \
	    diff -y --suppress-common-lines "$(OFETCH).sha1" "$(WGET).sha1" || true\
	  ; } && \
	  { cmp -b $(OFETCH) $(WGET) || \
		{ diff -y --suppress-common-lines $(OFETCH) $(WGET) \
		  | head -10 ; exit 1 ; } ; \
	  } ; \
	} && \
	echo ============
endef

network-test: cli ./_build/ofetch
	@ # Chunked:
	$(call compare_wget,lists-ocaml-org,"http://lists.ocaml.org/listinfo/platform/")
	@ # Content-Length:
	$(call compare_wget,robur,"http://robur.io/")
	$(call compare_wget,opam-frontpage,"http://opam.ocaml.org/")
	$(call compare_wget,hbsd-ftp,"http://ftp.hardenedbsd.org/")
	$(call compare_wget,mccs,"http://www.i3s.unice.fr/~cpjm/misc/mccs-1.1-srcs.tgz")
	  @ # this one reports Accept-Ranges: bytes:
	$(call compare_wget,debian-sha256,"http://ftp.nl.debian.org/debian/dists/stretch/main/installer-armhf/current/images/SHA256SUMS")
	@ # misc
	$(call compare_wget,pinata,"http://ownme.ipredator.se/")

clean:
	@ rm -fr ./_build/

# commented out: example of using temporary directories for downloads:
# $(eval TESTDIR := $(shell mktemp -d))
#@ rm -f "$(TESTDIR)/hbsd.ftp" ; \
#@ rmdir "$(TESTDIR)"
