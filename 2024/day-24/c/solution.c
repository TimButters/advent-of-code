#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_WIRES 350
#define MAX_GATES 250
#define LABEL_LENGTH 10

struct Gate {
    char input1[LABEL_LENGTH];
    char input2[LABEL_LENGTH];
    char output[LABEL_LENGTH];
    char type[4];
};

struct Wire {
    char label[LABEL_LENGTH];
    int value;
};

struct WireSet {
    struct Wire wires[MAX_WIRES];
    size_t num_wires;
};

int compare_wires(const void* wire1, const void* wire2)
{
    struct Wire* w1 = (struct Wire*)wire1;
    struct Wire* w2 = (struct Wire*)wire2;
    return strcmp(w1->label, w2->label);
}

struct WireSet build_zs(char wires[][LABEL_LENGTH], int* wire_values, size_t num_wires)
{
    struct WireSet wire_set;
    struct Wire this_wire;
    int num_zs = 0;
    for (int i = 0; i < num_wires; ++i) {
        if (wires[i][0] == 'z') {
            strcpy(this_wire.label, wires[i]);
            this_wire.value = wire_values[i];
            wire_set.wires[num_zs] = this_wire;
            num_zs++;
        }
    }
    wire_set.num_wires = num_zs;
    return wire_set;
}

size_t wire_index(char wires[][LABEL_LENGTH], size_t num_wires, char* wire)
{
    for (size_t i = 0; i < num_wires; ++i) {
        if (strcmp(wires[i], wire) == 0) {
            return i;
        }
    }
    return num_wires + 1;
}

struct Wire parse_wire(char* line)
{
    char* pch;
    char label[LABEL_LENGTH];
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

    tokens[4][strcspn(tokens[4], "\n")] = 0;

    struct Gate gate;
    strcpy(gate.input1, tokens[0]);
    strcpy(gate.type, tokens[1]);
    strcpy(gate.input2, tokens[2]);
    strcpy(gate.output, tokens[4]);
    return gate;
}

int add_wire(char* label, char wires[][LABEL_LENGTH], int* wire_values, size_t num_wires)
{
    size_t wire_idx = wire_index(wires, num_wires, label);
    if (wire_idx < num_wires) {
        return num_wires;
    }
    strcpy(wires[num_wires], label);
    wire_values[num_wires] = -1;
    return num_wires + 1;
}

int run_gate(struct Gate* gate, int* wire_values, char wires[][LABEL_LENGTH], size_t num_wires)
{
    int value;

    int input1 = wire_values[wire_index(wires, num_wires, gate->input1)];
    int input2 = wire_values[wire_index(wires, num_wires, gate->input2)];
    size_t output = wire_index(wires, num_wires, gate->output);

    if (input1 == -1 || input2 == -1) {
        return -1;
    }

    if (strcmp(gate->type, "AND") == 0) {
        value = input1 && input2;
    } else if (strcmp(gate->type, "OR") == 0) {
        value = input1 || input2;
    } else {
        value = input1 != input2;
    }

    wire_values[output] = value;
    return value;
}

bool complete_zs(char wires[][LABEL_LENGTH], int* wire_values, size_t num_wires)
{
    for (int i = 0; i < num_wires; ++i) {
        if (wires[i][0] == 'z' && wire_values[i] == -1) {
            return false;
        }
    }
    return true;
}

void run_gates(char wires[][LABEL_LENGTH], int* wire_values, struct Gate* gates, size_t num_wires, size_t num_gates)
{
    while (!complete_zs(wires, wire_values, num_wires)) {
        for (int i = 0; i < num_gates; ++i) {
            run_gate(&gates[i], wire_values, wires, num_wires);
        }
    }
}

void load_input(char* filename, char wires[][LABEL_LENGTH], int* wire_values, struct Gate* gates, size_t* num_wires, size_t* num_gates)
{
    FILE* fptr;
    char* line = NULL;
    size_t len = 0;
    ssize_t read = 0;

    fptr = fopen(filename, "r");
    if (fptr == NULL) {
        printf("Could not open file.\n");
        return;
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
            strcpy(wires[*num_wires], wire.label);
            wire_values[*num_wires] = wire.value;
            (*num_wires)++;
        } else {
            gates[*num_gates] = parse_gate(line);
            char wire1[LABEL_LENGTH];
            char wire2[LABEL_LENGTH];
            char wire3[LABEL_LENGTH];
            strcpy(wire1, gates[*num_gates].input1);
            strcpy(wire2, gates[*num_gates].input2);
            strcpy(wire3, gates[*num_gates].output);
            *num_wires = add_wire(wire1, wires, wire_values, *num_wires);
            *num_wires = add_wire(wire2, wires, wire_values, *num_wires);
            *num_wires = add_wire(wire3, wires, wire_values, *num_wires);
            (*num_gates)++;
        }
    }
    fclose(fptr);
}

int main(int argc, char** argv)
{
    char wires[MAX_WIRES][LABEL_LENGTH];
    int wire_values[MAX_WIRES];
    struct Gate gates[MAX_GATES];
    size_t num_wires = 0;
    size_t num_gates = 0;

    if (argc < 2) {
        printf("Provide an input file\n");
        return 1;
    }

    load_input(argv[1], wires, wire_values, gates, &num_wires, &num_gates);

    printf("Number of wires: %lu\n", num_wires);
    printf("Number of gates: %lu\n\n", num_gates);

    run_gates(wires, wire_values, gates, num_wires, num_gates);

    struct WireSet zs = build_zs(wires, wire_values, num_wires);
    qsort(zs.wires, zs.num_wires, sizeof(struct Wire), compare_wires);

    char answer[50];
    for (int i = 0; i < zs.num_wires; ++i) {
        answer[zs.num_wires - 1 - i] = zs.wires[i].value + '0';
    }

    printf("Part 1: %ld\n", strtol(answer, NULL, 2));

    return 0;
}
