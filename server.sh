#!/bin/bash

cd web

# Prepare links that are expected to be in the web/ folder.

if [ "$1" = "copy" ]
then
	LN="cp"
	LNF="cp -R"
	shift
else
	LN="ln -sf"
	LNF="ln -sf"
fi

$LN ../_build/default/js/main.js main.js
$LN ../translations.json translations.json

if [ ! -e doc ]
then
	if [ -e ../_build/default/_doc/_html ]
	then
		$LNF ../_build/default/_doc/_html doc
	fi
fi

if [ ! -e data ]
then
	$LNF ../data data
fi

# If being asked to run the server, run it.

if [ "$1" = "run" ]
then
	python -m SimpleHTTPServer
fi

