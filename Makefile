# Define installation arguments with optional prefix
INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
# Define examples with their descriptions
EXAMPLE_DESCRIPTIONS := \
	"example-building-ast:Demonstrates how to build AST" \
	"example-destructuring-ast:Demonstrates how to destructure an AST" \

.PHONY: help
help: ## Print this help message
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";
	@echo "Available examples:";
	@echo "";
	@for desc in $(EXAMPLE_DESCRIPTIONS); do \
		target=$$(echo $$desc | cut -d: -f1); \
		description=$$(echo $$desc | cut -d: -f2); \
		printf "  \033[36m%-30s\033[0m %s\n" "$$target" "$$description"; \
	done
	@echo "";

.PHONY: default
default: ## Build the project with auto-promote
	dune build --auto-promote @install

.PHONY: install
install: ## Install the project
	dune install $(INSTALL_ARGS)

.PHONY: uninstall
uninstall: ## Uninstall the project
	dune uninstall $(INSTALL_ARGS)

.PHONY: reinstall
reinstall: ## Reinstall the project
	uninstall reinstall

.PHONY: test
test: ## Run tests
	dune runtest

.PHONY: doc
doc: ## Build documentation
	dune build @doc

.PHONY: doc-dev
doc-dev: ## Build and watch documentation
	dune build @doc --watch & dune_pid=$$!; \
	trap 'kill $$dune_pid' EXIT; \
	sleep 2 && open _build/default/_doc/_html/index.html & \
	wait $$dune_pid

.PHONY: clean
clean: ## Clean the build artifacts
	dune clean

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions: ## Build for all supported OCaml versions
	dune build @install --workspace dune-workspace.dev --root .

.PHONY: opam-release
opam-release: ## Release the project using opam
	dune-release distrib --skip-build --skip-lint --skip-tests
	dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit

.PHONY: bench
bench: ## Run benchmarks
	dune build bench --profile release
	dune exec bench/bench.exe

.PHONY: $(TARGET)
example-%: ## Run example with specified target, e.g. make example-global-transformation
	opam exec -- dune exec $*-example
