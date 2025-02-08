CC = gcc
CFLAGS = -fsanitize=address -Wall

solution: $(wildcard *.c)
	$(CC) $(CFLAGS) $^ -o $@
