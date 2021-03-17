##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## Makefile 308reedpipes
##

NAME	=	309pollution

SRC		=	pollution_309.py

all:
	cp $(SRC) $(NAME)
	chmod 755 $(NAME)

tests_run:
	python3 -m unittest discover -v -s . -p "tests_pollution_309.py"

clean:
	echo "Nothing to clean"

fclean:	clean
	rm $(NAME)	\
	rm -f *~	\
	rm -f *#	\

re:	fclean clean all

.PHONY:	all re clean fclean
