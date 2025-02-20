#!/bin/bash

# Compile the Bison-generated C code
bison -d myanalyzer.y

# Compile the Flex-generated C code
flex mylexer.l

gcc -o comp lex.yy.c myanalyzer.tab.c cgen.c -lfl
./comp < correct1.ka
