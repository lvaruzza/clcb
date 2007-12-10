#!/bin/sh

# This file outputs a configuration file that could become
# $HOME/.sbclrc if there are no other Lisp packages used
# on that system. The option --userinit may indicate another
# location to the output of this script.

bindir=`dirname $0` 
rootdir=`pwd`

prefix=`echo $bindir | cut -f1 -d/`
if [ -z "$prefix" ]; then
	rootdir=`dirname $bindir`
else
	rootdir=$rootdir/`dirname $bindir`
fi

rootdir=`echo $rootdir|sed -e 's%/.$%%' -e 's%//%/%'`

if find /home/moeller/.sbcl > /dev/null; then
	# Albert, wirf mal 'nen Blick hierauf, bitte,
	# hiernach funktionierte (in-package :tinaa)
	# zwar, aber das liegt doch an etwas anderem, oder?
	#echo "(pushnew "$rootdir" :tinaa)"
	#echo "(asdf:oos 'asdf:load-op :tinaa)"
	echo "(in-package :tinaa)"
else
	echo ";;; Could not find tinaa on your system"
	echo
fi

cat<<EOUSERINIT
(pushnew "$rootdir" asdf:*central-registry*)
(pushnew "$rootdir/ensembl" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :clcb)
(asdf:oos 'asdf:load-op :clcb-ensembl)
(in-package :ensembl)
EOUSERINIT
