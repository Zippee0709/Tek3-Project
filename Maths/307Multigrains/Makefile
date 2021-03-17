##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## Makefile 307multigrains
##

NAME	=	307multigrains

SRC		=	multigrains_307.py

all:
	cp $(SRC) $(NAME)
	chmod 755 $(NAME)

tests_run:
	python3 test_multigrains_307.py

clean:
	echo "Nothing to clean"

fclean:	clean
	rm $(NAME)	\
	rm -f *~	\
	rm -f *#	\

re:	fclean clean all

.PHONY:	all re clean fclean
