all:
	happy -gca ParCmm.y
	alex -g LexCmm.x
	ghc --make TestCmm.hs -o TestCmm

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocCmm.ps

distclean: clean
	-rm -f DocCmm.* LexCmm.* ParCmm.* LayoutCmm.* SkelCmm.* PrintCmm.* TestCmm.* AbsCmm.* TestCmm ErrM.* SharedString.* ComposOp.* Cmm.dtd XMLCmm.* Makefile*
	

