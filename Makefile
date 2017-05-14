OCAMLOPT=ocamlopt

llg: llg.ml
	$(OCAMLOPT) -o "$@" -inline 20 -nodynlink "$<"
clean:
	rm -f llg llg.cmi llg.cmx llg.o llg.s
