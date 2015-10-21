# Buh

Bup routines in Haskell.

## Other executable

`hush` is a simple equivalent to `git hash-object --stdin -w` and `git cat-file
-p`:

```
> echo hello | GIT_OBJECT_DIRECTORY=/objects git hash-object --stdin -w
ce013625030ba8dba906f756967f9e9ca394464a
> GIT_OBJECT_DIRECTORY=/objects git cat-file -p ce013625030ba8dba906f756967f9e9ca394464a
hello
> hexdump /objects/ce/013625030ba8dba906f756967f9e9ca394464a 
0000000 0178 ca4b 4fc9 3052 c863 cd48 c9c9 02e7
0000010 1d00 04c5 0014                         
0000015
```

```
> echo hello | ./dist/build/hush/hush
ce013625030ba8dba906f756967f9e9ca394464a
> ./dist/build/hush/hush ce013625030ba8dba906f756967f9e9ca394464a
hello
> hexdump /objects/ce/013625030ba8dba906f756967f9e9ca394464a 
0000000 0178 ca4b 4fc9 3052 c863 cd48 c9c9 02e7
0000010 1d00 04c5 0014                         
0000015
```

Note: the created file's mode is not the same.
