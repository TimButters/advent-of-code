#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Gate {
    char input1[10];
    char input2[10];
    char output[10];
    char type[4];
};

struct Wire {
    char label[10];
    int value;
};

size_t wire_index(char** wires, size_t num_wires, char* wire)
{
    for (size_t i = 0; i < num_wires; ++i) {
        if (strcmp(wires[i], wire)) {
            return i;
        }
    }
    return num_wires + 1;
}

int run_gate(struct Gate* gate, int* wire_values, char** wires, size_t num_wires)
{
    int input1 = wire_values[wire_index(wires, num_wires, gate->input1)];
    int input2 = wire_values[wire_index(wires, num_wires, gate->input2)];
    if (input1 == -1 || input2 == -1) {
        return -1;
    }

    if (strcmp(gate->type, "AND") == 0) {
        return input1 && input2;
    } else if (strcmp(gate->type, "OR") == 0) {
        return input1 || input2;
    } else {
        return input1 != input2;
    }
}

struct Wire parse_wire(char* line)
{
    char* pch;
    char label[10];
    char value[5];

    size_t token_count = 0;
    pch = strtok(line, ":");
    strcpy(label, pch);
    pch = strtok(NULL, ":");
    strcpy(value, pch);

    struct Wire wire;
    strcpy(wire.label, label);
    wire.value = atoi(value);
    return wire;
}

struct Gate parse_gate(char* line)
{
    char* pch;
    char tokens[5][10];

    size_t token_count = 0;
    pch = strtok(line, " \t");
    while (pch != NULL) {
        strcpy(tokens[token_count], pch);
        pch = strtok(NULL, " \t");
        token_count++;
    }

    struct Gate gate;
    strcpy(gate.input1, tokens[0]);
    strcpy(gate.type, tokens[1]);
    strcpy(gate.input2, tokens[2]);
    strcpy(gate.output, tokens[4]);
    return gate;
}

int main(int argc, char** argv)
{
    size_t MAX_WIRES = 100;

    FILE* fptr;
    char* line = NULL;
    size_t len = 0;
    ssize_t read = 0;

    size_t num_wires = 0;
    size_t num_gates = 0;
    struct Gate gates[100];
    char wires[MAX_WIRES][100];
    int wire_values[MAX_WIRES];

    fptr = fopen("../test_input.txt", "r");
    if (fptr == NULL) {
        printf("Could not open file.\n");
        return 1;
    }

    struct Wire wire;
    bool first_section = true;
    while ((read = getline(&line, &len, fptr)) != -1) {
        if (strcmp(line, "\n") == 0) {
            first_section = false;
            continue;
        }

        if (first_section) {
            wire = parse_wire(line);
            strcpy(wires[num_wires], wire.label);
            wire_values[num_wires] = wire.value;
            num_wires++;
        } else {
            gates[num_gates] = parse_gate(line);
            num_gates++;
        }
    }

    fclose(fptr);

    printf("Number of wires: %lu\n", num_wires);
    for (int i = 0; i < num_wires; ++i) {
        printf("%s: %d\n", wires[i], wire_values[i]);
    }
    printf("\nNumber of gates: %lu\n", num_gates);
    for (int i = 0; i < num_gates; ++i) {
        printf("%s\t%s\t%s\t%s\n", gates[i].input1, gates[i].input2, gates[i].type, gates[i].output);
    }

    return 0;
}
