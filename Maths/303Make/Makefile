##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## Makefile 303make
##

NAME	=	303make

SRC		=	make_303.py

all:
	cp $(SRC) $(NAME)
	chmod 755 $(NAME)

tests_run:
	python3 test_make_303.py

clean:
	echo "Nothing to clean"

fclean:	clean
	rm $(NAME)	\
	rm -f *~	\
	rm -f *#	\

re:	fclean clean all

.PHONY:	all re clean fclean