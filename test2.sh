#!/bin/bash


list_fichiers=`ls tests/syntax/good`


echo "(***** GOOD TESTS SYNTAX ******)"

for fichier in $list_fichiers
do
	./prustc --parse_only tests/syntax/good/$fichier
done

list_fichiers=`ls tests/typing/good`


echo "(****** GOOD TESTS TYPING 1 *****)"

for fichier in $list_fichiers
do
	./prustc --parse_only tests/typing/good/$fichier
done
list_fichiers=`ls tests/typing2/good`


echo "(******  GOOD TESTS TYPING 2 ****)"

for fichier in $list_fichiers
do
	./prustc --parse_only tests/typing2/good/$fichier
done
list_fichiers=`ls tests/exec`


echo "(****** GOOD TESTS EXEC ******)"

for fichier in $list_fichiers
do
	./prustc --parse_only tests/exec/$fichier
done


echo "BAD TESTS"

for fichier in $list_fichiers
do
	./prustc --parse_only tests/syntax/bad/$fichier
done
