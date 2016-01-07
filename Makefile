all: latc_llvm

CABAL_FILE = latte.cabal
CONFIGURE = dist/setup-config
SOURCE = $(shell find src/Latte/ -name '*.lhs' -o -name '*.hs')
INSC_LLVM = latc_llvm
INSC_LLVM_BUILD = dist/build/latc_llvm/latc_llvm
RUNTIMEBC = lib/runtime.bc
RUNTIMELL = lib/runtime.ll

.PHONY: install configure build run clean

install:
	cabal install

$(RUNTIMEBC): $(RUNTIMELL)
	llvm-as -o $(RUNTIMEBC) $(RUNTIMELL)

$(CONFIGURE): $(CABAL_FILE)
	cabal update
	cabal install --only-dependencies
	cabal configure --enable-tests

configure: $(CONFIGURE)

build: $(CONFIGURE)
	cabal build

$(INSC_LLVM_BUILD): $(CONFIGURE) $(SOURCE) $(RUNTIMEBC)
	cabal build

$(INSC_LLVM): $(INSC_LLVM_BUILD)
	cp $(INSC_LLVM_BUILD) .

test: $(CONFIGURE)
	cabal test --show-details=streaming

run: $(CONFIGURE)
	cabal run

clean:
	rm -rf dist $(INSC_LLVM)
