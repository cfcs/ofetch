native: builddir lib ofetch_cli.ml
	@ cd _build/ && \
	set -x && \
	ocamlopt.opt -absname -unboxed-types -safe-string \
		-w +1..999-57 -warn-error +a-26-27-4-33 \
		-ccopt -fPIE -compact -verbose \
		unix.cmxa bigarray.cmxa ofetch.cmx \
		./ofetch_cli.ml -o ./ofetch && \
	{ { set +x ; } 2> /dev/null ; } && \
	strip ./ofetch && \
	ls -lh ./ofetch

builddir: *.ml
	@ mkdir -p _build/ && \
	  cp ./*.ml _build/

lib: builddir ofetch.ml
	@ cd _build/ && \
	ocamlopt.opt -absname -unboxed-types -safe-string \
		-w +1..999-57 -warn-error +a-26-27-4-33 \
		-ccopt -fPIE -compact -verbose \
		-c -linkall \
		bigarray.cmxa ./ofetch.ml

test: builddir lib ofetch.ml tests.ml
	cd _build/ && \
	cp ../tests.ml . && \
	ocamlopt.opt -absname -unboxed-types -safe-string \
		-w +1..999-57 -warn-error +a-26-27-4-33 \
		-ccopt -fPIE -compact \
		unix.cmxa bigarray.cmxa ./ofetch.cmx ./tests.ml -o ./tests && \
		./tests && echo "tests ran, all good"

debug: builddir ofetch.ml
	cd _build/ && \
	ocamlc.byte -absname -unboxed-types -safe-string \
		-w +1..999-57 -warn-error +a-26-27-4-33 \
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
	time wget -O $(WGET) "$2" && \
	time ./ofetch -v $(OFETCH) "$2" ; \
	{ { set +x ; } 2> /dev/null ; } && \
	{ \
 	  { sha1sum "$(OFETCH)" > "$(OFETCH).sha1" && \
	    sha1sum "$(WGET)" > "$(WGET).sha1" && \
	    diff -y --suppress-common-lines "$(OFETCH).sha1" "$(WGET).sha1" || true\
	  ; } && \
	  { cmp -b $(OFETCH) $(WGET) || \
		{ diff -y $(OFETCH) $(WGET) \
	          | head -10 ; exit 1 ; } ; \
	  } ; \
	} && \
	echo ============
endef

network-test: native ./_build/ofetch
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
