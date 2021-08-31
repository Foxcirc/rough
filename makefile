
build:
	cargo run

run:
	cargo run

test:
	cls
	cargo test -- --nocapture

commit:
	git add *
	git commit -m ...
