#! /bin/bash
export BUP_DIR=/source

bup ls
bup ls non-existing
bup ls master
bup ls master/latest
bup ls master/latest/buh.cabal
bup cat-file master/latest/buh.cabal
