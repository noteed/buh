#! /bin/bash

# Somehow validate the pack creation. It tries to create an index, extract
# the objects using `git cat-file`, and checkout the tree.

./dist/build/buh/buh pack
git index-pack /tmp/t.pack
git verify-pack -v /tmp/t.pack

git init --bare /tmp/repo
cp /tmp/t.{idx,pack} /tmp/repo/objects/pack/
cd /tmp/repo

git cat-file --batch-check << EOF
32ab47487f560b11fdc758eedd9b43ee7aeb8684
85368e63a1ef657360e107818b529727b73b001e
709149cd69d4e13c8740e5bb3d832f97fcb08878
EOF

echo 709149cd69d4e13c8740e5bb3d832f97fcb08878 > refs/heads/master
git log

mkdir work
GIT_WORK_TREE=work git checkout master
ls work

cd -
rm -rf /tmp/repo
