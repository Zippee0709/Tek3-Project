##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## Makefile 308reedpipes
##

NAME	=	308reedpipes

SRC		=	reedpipes_308.py

all:
	cp $(SRC) $(NAME)
	chmod 755 $(NAME)

tests_run:
	python3 -m unittest discover -v -s . -p "tests_reedpipes_308.py"

clean:
	echo "Nothing to clean"

fclean:	clean
	rm $(NAME)	\
	rm -f *~	\
	rm -f *#	\

re:	fclean clean all

.PHONY:	all re clean fclean
