#!/bin/sh

if [ ! -d ./src ]; then
  if [ ! -d ../src ]; then
    echo 'cant find src';
    exit
  fi
else
  cd src/
fi

for f in ../test/*.smc; do
    ./runmod.scm sexpr-to-parseobj.scm <$f | ./runmod.scm macexpand.scm > `echo $f | sed s/\.smc$/\.smo/`;
done

