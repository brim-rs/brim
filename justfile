#!/usr/bin/env -S just --justfile

set windows-shell := ["powershell"]
set shell := ["bash", "-cu"]

_default:
    @just --list -u

ready:
    just fmt
    just check
    just test
    just lint
    just doc
    git status

fix:
    cargo clippy --fix --allow-staged --no-deps
    just fmt
    git status

check:
    cargo check --workspace --all-features --all-targets --locked

lint:
    cargo clippy --workspace --all-targets --all-features -- --deny warnings

test:
    cargo test

fmt:
    cargo shear --fix
    cargo fmt --all
    dprint fmt

[unix]
doc:
    RUSTDOCFLAGS='-D warnings' cargo doc --no-deps --document-private-items

[windows]
doc:
    $Env:RUSTDOCFLAGS='-D warnings'; cargo doc --no-deps --document-private-items

run args='':
    cd playground; cargo run -p brim-cli run {{ args }}

docs:
    cd docs; bun dev

setup:
    cd cli; cargo build; cd ../docs; pnpm install
