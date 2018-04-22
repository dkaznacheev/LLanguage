#! /bin/bash

RES=$((dpkg-query -W -f='${Status}' ghc) 2>&1)
if [[ "$RES" =~ .*no.* ]]; then
    sudo apt-get install ghc;
fi
if [ "$1" == "test" ]; then
    ghc test.hs
    ./test
else
    ghc lexer.hs
fi
