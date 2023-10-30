all:
	cabal build
	cabal install --installdir="." --overwrite-policy=always

.PHONY: clean

clean:
	rm -f insc_jvm
	rm -f insc_llvm
	cabal clean