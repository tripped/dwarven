.PHONY = clean
GHCOPTS = -outputdir obj
GHC = ghc

all: decode

decode: decode.hs
	$(GHC) $(GHCOPTS) $@

clean:
	rm -rf obj/ decode
