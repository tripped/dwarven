.PHONY = clean
GHCOPTS = -outputdir obj
GHC = ghc

all: decode parseraw

decode: decode.hs
	$(GHC) $(GHCOPTS) $@

parseraw: parseraw.hs
	$(GHC) $(GHCOPTS) $@

clean:
	rm -rf obj/ decode
