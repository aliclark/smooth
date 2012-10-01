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
    fout=`echo $f | sed s/\.smc$/\.smo/ | sed s/\.scm$/\.smo/`

    progs="../target/comments.scm ./runmod.scm sexpr-to-parseobj.scm macexpand.scm"
    progchange=''

    for g in $progs; do
        if [ -f $fout ] && [ "`stat -c '%Y' $g`" -ge "`stat -c '%Y' $fout`" ]; then
            echo $g is too new
            progchange=1
        fi
    done

    if [ "$progchange" != "" ] || (! ([ -f $fout ] && [ "`stat -c '%Y' $fout`" -gt "`stat -c '%Y' $f`" ])); then
        echo "precompiling dependency $f to $fout" >&2
        echo "  comment stripping" >&2
        gsi ../target/comments.scm <$f >$fout.1
        echo "  creating parse objects" >&2
        ./runmod.scm sexpr-to-parseobj.scm <$fout.1 >$fout.2
        rm $fout.1
        echo "  macro expansion" >&2
        ./runmod.scm macexpand.scm <$fout.2 > $fout
        rm $fout.2
    else
        echo "dependency $fout is precompiled" >&2
    fi
done

srcpre=`echo $f | sed s/\.smc$/\.smo/ | sed s/\.scm$/\.smo/`
targ=../target/`echo $srcpre | sed s/\.smo$/\.scm/`

# .smo files are ready to be load'ed
echo "compiling srcfile $srcfile to $targ.out" >&2

echo "  loading included files" >&2
./runmod.scm load.scm <$srcpre >$targ.out.1

echo "  expanding main variable" >&2
./runmod.scm varexpand.scm <$targ.out.1 >$targ.out.2
rm $targ.out.1

echo "  beta reducing" >&2
./runmod.scm beta.scm <$targ.out.2 >$targ.out.3
rm $targ.out.2

echo "  outputting target code" >&2
./runmod.scm output-scm.scm <$targ.out.3 >$targ.out
rm $targ.out.3

if ! diff $targ.out $targ ; then
    mv $targ.out $targ
else
    echo 'target has not changed'
    rm $targ.out
fi
