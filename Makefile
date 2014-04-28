all: build

build: makorel

makorel: makorel.native
	mv $< $@

makorel.native:
	ocamlbuild -use-ocamlfind -package str,unix $@

clean:
	ocamlbuild -clean
