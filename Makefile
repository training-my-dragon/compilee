.PHONY: run

FILE="ccc-example/code1.ccc" "ccc-example/code2.ccc" "ccc-example/code3.ccc"
SHELL := /bin/bash
run:
	@echo "Instalando rustup..."
	curl https://sh.rustup.rs -sSf | sh -s -- -y
	source "${HOME}/.cargo/env"
	cargo run ${FILE}
