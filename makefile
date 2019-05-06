default: Main
	@echo
	@echo this is going to take a long time
	./Main script

Main: Main.hs Parse.hs Transform.hs Line.hs Solids.hs Screen.hs DrawMats.hs
	ghc -dynamic -O -O2 Main.hs

build: Main

clean:
	rm *.hi *.o Main .tempimg.ppm

imgclean:
	rm *.ppm *.png .tempimg.ppm

run:
	./Main script
