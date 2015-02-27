OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

all: ../samples/test1 ../samples/testparse ../bin/gnirut ../bin/gnirutc ../bin/gnipp ../bin/rutdump ../bin/rutrev ../bin/rutrun ../bin/rutfs

TEST1_OBJS=matching.cmo stab.cmo rtm.cmo test1.cmo

../samples/test1: $(TEST1_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

TESTPARSE_OBJS=matching.cmo instr.cmo gniyacc.cmo gnilex.cmo testparse.cmo

../samples/testparse: $(TESTPARSE_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

GNIRUT_OBJS=matching.cmo stab.cmo rtm.cmo instr.cmo gniyacc.cmo gnilex.cmo interp.cmo gnirut.cmo

../bin/gnirut: $(GNIRUT_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

GNIRUTC_OBJS=matching.cmo stab.cmo rtm.cmo instr.cmo gniyacc.cmo gnilex.cmo interp.cmo gnirutc.cmo

../bin/gnirutc: $(GNIRUTC_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

GNIPP_OBJS=matching.cmo stab.cmo rtm.cmo instr.cmo gniyacc.cmo gnilex.cmo interp.cmo gnipp.cmo

../bin/gnipp: $(GNIPP_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

RUTDUMP_OBJS=matching.cmo stab.cmo rtm.cmo rutdump.cmo

../bin/rutdump: $(RUTDUMP_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

RUTREV_OBJS=matching.cmo stab.cmo rtm.cmo rutrev.cmo

../bin/rutrev: $(RUTREV_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

RUTRUN_OBJS=matching.cmo stab.cmo rtm.cmo rutrun.cmo

../bin/rutrun: $(RUTRUN_OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $^

RUTFS_OBJS=matching.cmx stab.cmx rtm.cmx rutfs.cmx

../bin/rutfs: $(RUTFS_OBJS)
	$(OCAMLOPT) -o $@ $(OCAMLFLAGS) graphics.cmxa $^


.SUFFIXES: .ml .mli .mll .mly .cmo .cmi .cmx .rut .gni .tex

.gni.rut: gnirutc
	../bin/gnirutc $< $@

.gni.tex: gnipp
	../bin/gnipp $< $@

.mll.ml:
	$(OCAMLLEX) $<

.mll.mli:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) $<

.mly.mli:
	$(OCAMLYACC) $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	rm -f ../samples/test1 ../samples/testparse
	rm -f *.cm[iox] *.o gnilex.ml gniyacc.mli gniyacc.ml

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml *.mll *.mly > .depend

include .depend
