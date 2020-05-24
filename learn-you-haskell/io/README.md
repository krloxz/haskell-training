IO Examples
===========

```shell
ghc --make capslocker
cat haiku.txt | ./capslocker

ghc --make shortlinesonly
cat shortlines.txt | ./shortlinesonly

ghc --make todo.hs
./todo view todo.txt
./todo add todo.txt "A sample task"
./todo remove todo.txt 2

ghc --make countlines.hs
./countlines todo.txt
./countlines notFoundFile
```

