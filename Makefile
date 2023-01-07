##
## EPITECH PROJECT, 2022
## B-FUN-400-LIL-4-1-compressor-thomas.gireaudot
## File description:
## Makefile
##

NAME 	= imageCompressor

all : $(NAME)

$(NAME):
	stack install --local-bin-path .
	mv imageCompressor-exe imageCompressor

clean:
	rm -f

fclean: clean
	rm -f *.vgcore $(NAME)

re : fclean all