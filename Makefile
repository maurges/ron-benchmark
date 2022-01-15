all: input/list_of_ints.ron haskell-exe rust-exe
	./haskell-exe $<
	./rust-exe $<

input/%.ron: generate/%.hs
	./$^

haskell-exe: haskell/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/bench-exe/bench-exe
	ln -s -T $^ $@
haskell/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/bench-exe/bench-exe: haskell/Main.hs
	( cd haskell; stack build )

rust-exe: ./rust/target/release/rust
	ln -s -T $^ $@
./rust/target/release/rust: rust/src/main.rs
	( cd rust; cargo build --release )
