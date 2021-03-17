##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## Makefile 305Construction
##

NAME	=	305construction

SRC		=	construction_305.py

all:
	cp $(SRC) $(NAME)
	chmod 755 $(NAME)

tests_run:
	python3 test_construction_305.py

clean:
	echo "Nothing to clean"

fclean:	clean
	rm $(NAME)	\
	rm -f *~	\
	rm -f *#	\

re:	fclean clean all

.PHONY:	all re clean fclean
