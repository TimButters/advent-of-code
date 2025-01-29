#include <stdio.h>
#include <string.h>

#define MAX_KEYS 300
#define MAX_LOCKS 300
#define NUM_PINS 5

typedef struct {
    size_t num_keys;
    size_t num_locks;
    int keys[MAX_KEYS][NUM_PINS];
    int locks[MAX_LOCKS][NUM_PINS];
} LockSet;

LockSet load_input(char* filename)
{
    FILE* fptr;
    char* line;
    size_t len;
    size_t num_locks = 0;
    size_t num_keys = 0;
    LockSet lockset;

    fptr = fopen(filename, "r");
    if (fptr == NULL) {
        printf("Could not open %s", filename);
    }

    char first_row = 1;
    char lock = 1;
    while (getline(&line, &len, fptr) != -1) {
        if (first_row && strcmp(line, "#####\n") == 0) {
            first_row = 0;
            lock = 1;
            num_locks++;
            continue;
        }

        if (first_row) {
            first_row = 0;
            lock = 0;
            num_keys++;
            for (int i = 0; i < NUM_PINS; ++i) {
                lockset.keys[num_keys - 1][i] = -1;
            }
        }

        if (strcmp(line, "\n") == 0) {
            first_row = 1;
            continue;
        }

        for (int i = 0; i < NUM_PINS; ++i) {
            if (line[i] == '#') {
                if (lock) {
                    lockset.locks[num_locks - 1][i]++;
                } else {
                    lockset.keys[num_keys - 1][i]++;
                }
            }
        }
    }
    fclose(fptr);

    lockset.num_keys = num_keys;
    lockset.num_locks = num_locks;

    return lockset;
}

int main(int argc, char** argv)
{
    LockSet lockset = load_input(argv[1]);
    printf("Locks: %zu\tKeys: %zu\n", lockset.num_locks, lockset.num_keys);

    int num_valid_pairs = 0;
    char breached = 0;
    for (int i = 0; i < lockset.num_locks; ++i) {
        for (int j = 0; j < lockset.num_keys; ++j) {
            for (int k = 0; k < NUM_PINS; ++k) {
                if (lockset.locks[i][k] + lockset.keys[j][k] > 5) {
                    breached = 1;
                }
            }
            if (!breached) {
                num_valid_pairs++;
            } else {
                breached = 0;
            }
        }
    }
    printf("Part 1: %d\n", num_valid_pairs);

    return 0;
}
