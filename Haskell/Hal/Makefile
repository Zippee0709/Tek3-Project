##
## EPITECH PROJECT, 2020
## funEvalExpr
## File description:
## Makefile
##

NAME = hal

SRC =  app/Main.hs \

OBJ = $(SRC:.hs=.o)

OBJS = $(SRC:.hs=.hi)

SETUP = Setup.o \
		Setup.hi \

all : $(NAME)

$(NAME) : $(SRC)
	stack build
	cp `stack path --local-install-root`/bin/$(NAME)-exe ./$(NAME)

run_tests : 
	stack test

clean :
	stack clean

fclean : clean
	$(RM) $(NAME)
	stack purge

re: fclean all

.PHONY : all clean fclean re
