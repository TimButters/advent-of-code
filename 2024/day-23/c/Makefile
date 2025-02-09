CC = gcc
CFLAGS = -fsanitize=address -Wall -g

solution: $(wildcard *.c)
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f solution
