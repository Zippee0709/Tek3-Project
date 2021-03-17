##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## Makefile 301dannon
##

all: challenge01 challenge02 challenge03 challenge04 challenge05 challenge06 challenge07 challenge08

challenge01:
	cp source01.py challenge01
	chmod 755 challenge01

challenge02:
	cp source02.py challenge02
	chmod 755 challenge02

challenge03:
	cp source03.py challenge03
	chmod 755 challenge03

challenge04:
	cp source04.py challenge04
	chmod 755 challenge04

challenge05:
	cp source05.py challenge05
	chmod 755 challenge05

challenge06:
	cp source06.py challenge06
	chmod 755 challenge06

challenge07:
	cp source07.py challenge07
	chmod 755 challenge07

challenge08:
	cp source08.py challenge08
	chmod 755 challenge08

tests_challenge01:
	python3 -m unittest discover -v -s . -p "tests_challenge01.py"

tests_challenge02:
	python3 -m unittest discover -v -s . -p "tests_challenge02.py"

tests_challenge03:
	python3 -m unittest discover -v -s . -p "tests_challenge03.py"

tests_challenge04:
	python3 -m unittest discover -v -s . -p "tests_challenge04.py"

tests_challenge07:
	python3 -m unittest discover -v -s . -p "tests_challenge04.py"

tests_run: tests_challenge01 tests_challenge02 tests_challenge03 tests_challenge04

clean:
	echo "Nothing to clean"

fclean:	clean
	rm challenge*	\
	rm -f *~	\
	rm -f *#	\

re:	fclean all

.PHONY:	all re clean fclean