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

cat<<EOUSERINIT
(pushnew "$rootdir" asdf:*central-registry*)
(pushnew "$rootdir/ensembl" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :clcb)
(asdf:oos 'asdf:load-op :clcb-ensembl)
(in-package :ensembl)
EOUSERINIT
