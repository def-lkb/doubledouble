PKG = doubledouble
OTHER = ../META doubledouble.ml doubledouble.mli
TARGETS = doubledouble.cmi doubledouble.cmo doubledouble.cmx

RUNTIME = native
TESTS = DoubleDoubleIOTest.$(RUNTIME) DoubleDoubleComputeTest.$(RUNTIME) DoubleDoubleBasicTest.$(RUNTIME)

all:
	ocamlbuild $(TARGETS)

clean:
	ocamlbuild -clean

install: all
	cd _build && ocamlfind install $(PKG) $(OTHER) $(TARGETS)

uninstall:
	ocamlfind remove $(PKG)

check test: $(TESTS)

.PHONY: all check test install $(TESTS)

$(TESTS):
	ocamlbuild -tag debug $@
	OCAMLRUNPARAM=b ./$@
