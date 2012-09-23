#!/bin/sh

# builds target/comments.scm from src/comments.smc

# must be run from either the 

if [ ! -d ./src ]; then
  if [ ! -d ../src ]; then
    echo 'cant find src';
    exit
  fi
else
  cd src/
fi

srcfile=comments.smc

# Ideally we would teach "load" to determine dependencies,
# and then precompile those immediately before load'ing them.
deps="lambda.scm bool.scm pair.scm numeral.scm list.scm basicio.scm $srcfile"

for f in $deps; do
    echo "precompiling dependency $f to" `echo $f | sed s/\.smc$/\.smo/ | sed s/\.scm$/\.smo/` >&2
    gsi ../target/comments.scm <$f | ./runmod.scm sexpr-to-parseobj.scm | ./runmod.scm macexpand.scm > `echo $f | sed s/\.smc$/\.smo/ | sed s/\.scm$/\.smo/`
done

srcpre=`echo $f | sed s/\.smc$/\.smo/ | sed s/\.scm$/\.smo/`

# .smo files are ready to be load'ed
echo "compiling srcfile $srcfile" >&2
./runmod.scm load.scm <$srcpre | ./runmod.scm varexpand.scm | ./runmod.scm output-scm.scm >../target/`echo $srcpre | sed s/\.smo$/\.scm/`.out

# Only do this once you are sure it is correct, or the previous version is well backed up.
# It would be a pita to lose a working copy of the program.

#mv ../target/`echo $srcpre | sed s/\.smo$/\.scm/`.out `echo $srcpre | sed s/\.smo$/\.scm/`
