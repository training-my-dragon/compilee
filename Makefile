.PHONY: run

FILE="ccc-example/code1.ccc" "ccc-example/code2.ccc" "ccc-example/code3.ccc"
SHELL := /bin/bash

install:
	@echo "Instalando rustup..."
	curl https://sh.rustup.rs -sSf | sh -s -- -y
	source "${HOME}/.cargo/env"

run:
	cargo run ${FILE}
