#!/usr/bin/env -S just --justfile

set windows-shell := ["powershell"]
set shell := ["bash", "-cu"]

_default:
    @just --list -u

ready:
    just fmt
    just check
    just fix
    just test
    just doc
    git status

check:
    cargo check --workspace --all-features --all-targets

test:
    cargo test

fmt:
    cargo fmt --all

[unix]
doc:
    RUSTDOCFLAGS='-D warnings' cargo doc --no-deps --document-private-items

[windows]
doc:
    $Env:RUSTDOCFLAGS='-D warnings'; cargo doc --no-deps --document-private-items

fix:
    cargo fix --allow-dirty
    just fmt

run args='':
    cd playground; cargo run -p brim-cli run {{ args }}
