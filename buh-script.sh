#! /bin/bash
./dist/build/buh/buh ls
./dist/build/buh/buh ls non-existing
./dist/build/buh/buh ls master
./dist/build/buh/buh ls master/latest
./dist/build/buh/buh ls master/latest/buh.cabal
./dist/build/buh/buh cat-file master/latest/buh.cabal
