#! /bin/bash

# Somehow validate the pack creation. It tries to extract the objects using
# `git cat-file`, and checkout the tree.

./dist/build/buh/buh init /tmp/repo
cd /tmp/repo
$HOME/projects/buh/dist/build/buh/buh pack

git verify-pack -v objects/pack/p1.pack
git verify-pack -v objects/pack/p2.pack

git cat-file --batch-check << EOF
51c9ddc4a58cca9a62002120d25480201e7b8cc8
09f8177e9ea464d8a06f880ebd5575ae42f52cda
e6b9d54befa4e9c72ce8463c54b1e78737df3eba
EOF

git log
git log -p new-branch

mkdir work
GIT_WORK_TREE=work git checkout master
ls work
GIT_WORK_TREE=work git checkout new-branch
ls work

cd -
rm -rf /tmp/repo
