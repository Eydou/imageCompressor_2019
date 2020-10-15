##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile for clean.
##

CC      =       stack build

COPY_EXE      =       --copy-bins --local-bin-path .

all:
	@$(CC) $(COPY_EXE)
	@echo -e " -> \e[96mCompilation ok\033[0m"
clean:
	@stack clean --full
	@rm imageCompressor.cabal
	@echo -e " -> \e[96mIs Clean\033[0m"

fclean: clean
		@rm imageCompressor

re:     fclean all

.PHONY: all clean fclean
