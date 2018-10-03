INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	dune build --auto-promote @cinaps @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

test:
	dune runtest

clean:
	dune clean

all-supported-ocaml-versions:
	dune build @install --workspace dune-workspace.dev --root .

.PHONY: default install uninstall reinstall clean test
.PHONY: all-supported-ocaml-versions
