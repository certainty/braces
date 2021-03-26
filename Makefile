COLOR ?= always # Valid COLOR options: {always, auto, never}
CARGO = cargo --color $(COLOR)

.PHONY: all bench build check clean doc install run test update

all: build

bench:
	$(CARGO) bench

build:
	$(CARGO) build

debug_build:
	$(CARGO) build --features debug_vm

check:
	$(CARGO) check

clean:
	$(CARGO) clean

doc:
	$(CARGO) doc

install: build
	$(CARGO) install

run: 
	$(CARGO) run --bin vm

debug_run: 
	$(CARGO) run --features debug_vm --bin vm 


test: build
	$(CARGO) test -- --nocapture

update:
	$(CARGO) update
