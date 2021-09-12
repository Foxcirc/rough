
build:
	cargo +nightly run

run:
	cargo +nightly run

test:
	cls
	cargo +nightly test -- --nocapture

install: build
