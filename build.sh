#! /bin/bash

RES=$((dpkg-query -W -f='${Status}' ghc) 2>&1)
if [[ "$RES" =~ .*no.* ]]; then
    sudo apt-get install ghc;
fi
if [ "$1" == "test" ]; then
    ghc test.hs
    ./test
elif [ "$1" == "lexer" ]; then
    ghc lexer.hs
else
    ghc parser.hs
fi
