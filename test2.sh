#!/bin/bash


list_fichiers=`ls tests/syntax/good`


echo "GOOD TESTS"

for fichier in $list_fichiers
do
	./prustc --parse_only tests/syntax/good/$fichier
done

list_fichiers=`ls tests/syntax/bad`


echo "BAD TESTS"

for fichier in $list_fichiers
do
	./prustc --parse_only tests/syntax/bad/$fichier
done
