.DEFAULT_GOAL := all


.PHONY: all
all:
	opam exec -- dune build --root . @install

.PHONY: deps
deps: ## Install development dependencies
	opam install -y dune-release ocamlformat=0.19.0 utop ocaml-lsp-server merlin
	opam install --deps-only --with-test --with-doc -y .

.PHONY: create_switch
create_switch: ## Create an opam switch without any dependency
	opam switch create . --no-install -y

.PHONY: switch
switch: ## Create an opam switch and install development dependencies
	opam install . --deps-only --with-doc --with-test -y
	opam install -y dune-release ocamlformat=0.19.0 utop ocaml-lsp-server merlin

.PHONY: lock
lock: ## Generate a lock file
	opam lock -y .

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: install
install: all ## Install the packages on the system
	opam exec -- dune install --root .

.PHONY: uninstall
uninstall: ## Uninstall the packages from the system
	opam exec -- dune uninstall --root .

.PHONY: test
test: ## Run the unit tests
	opam exec -- dune runtest --root .

.PHONY : coverage
coverage : ## Generate coverage report
	find . -name '*.coverage' | xargs rm -f
	opam exec -- dune runtest --root . --instrument-with bisect_ppx --force
	opam exec -- bisect-ppx-report html
	opam exec -- bisect-ppx-report summary
	@echo See _coverage/index.html

.PHONY: bench
bench: ## Run benchmarks
	opam exec -- dune build --root . --profile=release
	opam exec -- dune exec --root . bench/bench.exe -- -quota 5

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: doc
doc: ## Generate odoc documentation
	opam exec -- dune build --root . @doc

.PHONY: servedoc
servedoc: doc ## Open odoc documentation with default web browser
	open _build/default/_doc/_html/index.html

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt || true

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	opam exec -- dune build --root . --watch

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

.PHONY: release
release: all ## Run the release script 
	opam exec -- dune-release tag
	opam exec -- dune-release distrib
	opam exec -- dune-release publish distrib -y
	opam exec -- dune-release opam pkg
	opam exec -- dune-release opam submit --no-auto-open -y
