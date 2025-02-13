##
## EPITECH PROJECT, 2023
## compressor
## File description:
## Makefile
##

NAME    =   imageCompressor

EXECUTABLE_PATH = $(shell stack path --local-install-root)/bin/compressor-exe

all:    $(NAME) copy_exec

$(NAME):
	stack build

copy_exec:
	cp $(EXECUTABLE_PATH) ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)
	stack purge

re: fclean all

.PHONY: all copy_exec clean fclean re
