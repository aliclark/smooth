#!/bin/sh

# builds target/comments.scm from src/comments.smc

# must be run from either the 

if [ ! -d ./src ]; then
  if [ ! -d ../src ]; then
    echo 'cant find src' >&2
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
            echo $g has changed >&2
            progchange=1
        fi
    done

    if [ "$progchange" != "" ] || (! ([ -f $fout ] && [ "`stat -c '%Y' $fout`" -gt "`stat -c '%Y' $f`" ])); then
        echo "precompiling dependency $f to $fout" >&2

        echo -n "  comment stripping     " >&2
        dur=`2>&1 /usr/bin/time -f "%e" gsi ../target/comments.scm <$f >$fout.1`
        printf " $dur secs\n" >&2

        echo -n "  creating parse objects" >&2
        dur=`2>&1 /usr/bin/time -f "%e" ./runmod.scm sexpr-to-parseobj.scm <$fout.1 >$fout.2`
        rm $fout.1
        printf " $dur secs\n" >&2

        echo -n "  macro expansion       " >&2
        dur=`2>&1 /usr/bin/time -f "%e" ./runmod.scm macexpand.scm <$fout.2 > $fout`
        rm $fout.2
        printf " $dur secs\n" >&2
    else
        printf "dependency $fout\tis precompiled\n" >&2
    fi
done

srcpre=`echo $f | sed s/\.smc$/\.smo/ | sed s/\.scm$/\.smo/`
targ=../target/`echo $srcpre | sed s/\.smo$/\.scm/`

# .smo files are ready to be load'ed
echo "compiling srcfile $srcfile to $targ.out" >&2

echo -n "  loading included files " >&2
dur=`2>&1 /usr/bin/time -f "%e" ./runmod.scm load.scm <$srcpre >$targ.out.1`
printf " $dur secs\n" >&2

echo -n "  expanding main variable" >&2
dur=`2>&1 /usr/bin/time -f "%e" ./runmod.scm varexpand.scm <$targ.out.1 >$targ.out.2`
rm $targ.out.1
printf " $dur secs\n" >&2

echo -n "  beta reducing          " >&2
dur=`2>&1 /usr/bin/time -f "%e" ./runmod.scm beta.scm <$targ.out.2 >$targ.out.3`
rm $targ.out.2
printf " $dur secs\n" >&2

echo -n "  debrujin conversion    " >&2
dur=`2>&1 /usr/bin/time -f "%e" ./runmod.scm debrujin.scm <$targ.out.3 >$targ.out.4`
rm $targ.out.3
printf " $dur secs\n" >&2

echo -n "  deduplication          " >&2
dur=`2>&1 /usr/bin/time -f "%e" ./runmod.scm dedup.scm <$targ.out.4 >$targ.out.5`
rm $targ.out.4
printf " $dur secs\n" >&2

echo -n "  dedebrujin conversion  " >&2
dur=`2>&1 /usr/bin/time -f "%e" ./runmod.scm dedebrujin.scm <$targ.out.5 >$targ.out.6`
rm $targ.out.5
printf " $dur secs\n" >&2

echo -n "  outputting target code " >&2
dur=`2>&1 /usr/bin/time -f "%e" ./runmod.scm output-scm.scm <$targ.out.6 >$targ.out`
rm $targ.out.6
printf " $dur secs\n" >&2

if ! diff $targ.out $targ >/dev/null 2>&1 ; then
    mv $targ.out $targ
else
    echo 'target has not changed' >&2
    rm $targ.out
fi
