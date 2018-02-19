TARGET=target
EXE=$(TARGET)/run-with-ssm
DIST_EXE=$(EXE)-$(shell uname -s)-$(shell uname -m)
DIST_EXE_SIG=$(DIST_EXE).sig

build:
	stack build run-with-ssm

build-prof:
	stack build --profile --ghc-options="-rtsopts -fprof-auto" run-with-ssm

install:
	stack install run-with-ssm

bindist:
	mkdir -p $(TARGET)
	stack --local-bin-path $(TARGET) install $(STACK_OPTS) run-with-ssm
	upx --best $(EXE)
	mv $(EXE) $(DIST_EXE)
	gpg --output $(DIST_EXE_SIG) --detach-sign $(DIST_EXE)

clean:
	stack clean
	rm -rf target

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies


.PHONY: build build-prof clean tags sources

