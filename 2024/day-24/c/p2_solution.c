#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_WIRES 350
#define MAX_GATES 250
#define LABEL_LENGTH 10

typedef struct {
    char input1[LABEL_LENGTH];
    char input2[LABEL_LENGTH];
    char output[LABEL_LENGTH];
    char type[4];
    int group;
} Gate;

typedef struct {
    Gate gates[MAX_GATES];
    size_t num_gates;
} GateSet;

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

Gate parse_gate(char* line)
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

    Gate gate;
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

GateSet load_input(char* filename, char wires[][LABEL_LENGTH], int* wire_values, size_t* num_wires)
{
    FILE* fptr;
    char* line = NULL;
    size_t len = 0;
    ssize_t read = 0;
    GateSet gates;
    gates.num_gates = 0;

    fptr = fopen(filename, "r");
    if (fptr == NULL) {
        printf("Could not open file.\n");
        return gates;
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
            gates.gates[gates.num_gates] = parse_gate(line);
            char wire1[LABEL_LENGTH];
            char wire2[LABEL_LENGTH];
            char wire3[LABEL_LENGTH];
            strcpy(wire1, gates.gates[gates.num_gates].input1);
            strcpy(wire2, gates.gates[gates.num_gates].input2);
            strcpy(wire3, gates.gates[gates.num_gates].output);
            *num_wires = add_wire(wire1, wires, wire_values, *num_wires);
            *num_wires = add_wire(wire2, wires, wire_values, *num_wires);
            *num_wires = add_wire(wire3, wires, wire_values, *num_wires);
            gates.num_gates++;
        }
    }
    fclose(fptr);

    return gates;
}

size_t gate_index_by_output(GateSet gates, char* wire)
{
    for (size_t i = 0; i < gates.num_gates; ++i) {
        if (strcmp(gates.gates[i].output, wire) == 0) {
            return i;
        }
    }
    return gates.num_gates + 1;
}

size_t gate_index_by_input(GateSet gates, char* wire, Gate** buffer)
{
    size_t num_found = 0;
    for (size_t i = 0; i < gates.num_gates; ++i) {
        if (strcmp(gates.gates[i].output, wire) == 0) {
            buffer[num_found] = &gates.gates[i];
            num_found++;
        }
    }
    return num_found;
}

void label_gates(GateSet* gates)
{
    int group = 1;
    char* wire = "z01";
    Gate* buffer[2];
    size_t idx_xor1;
    size_t idx_xor2;
    size_t idx_and1;
    size_t idx_and2;

    idx_xor1 = gate_index_by_output(*gates, wire);

    if (strcmp(gates->gates[idx_xor1].type, "XOR") != 0) {
        printf("Incorrect gate %s\n", gates->gates[idx_xor1].type);
        return;
    }

    printf("Correct gate found.\n");
    gates->gates[idx_xor1].group = group;

    size_t test_idx1 = gate_index_by_output(*gates, gates->gates[idx_xor1].input1);
    size_t test_idx2 = gate_index_by_output(*gates, gates->gates[idx_xor1].input2);

    if (strcmp(gates->gates[test_idx1].type, "XOR") == 0) {
        gates->gates[test_idx1].group = group;
        idx_xor2 = test_idx1;
    } else if (strcmp(gates->gates[test_idx2].type, "XOR") == 0) {
        gates->gates[test_idx2].group = group;
        idx_xor2 = test_idx2;
    } else {
        printf("No suitable gate found\n");
        return;
    }
}

int main(int argc, char** argv)
{
    char wires[MAX_WIRES][LABEL_LENGTH];
    int wire_values[MAX_WIRES];
    size_t num_wires = 0;
    GateSet gates;

    if (argc < 2) {
        printf("Provide an input file\n");
        return 1;
    }

    gates = load_input(argv[1], wires, wire_values, &num_wires);

    printf("Number of wires: %lu\n", num_wires);
    printf("Number of gates: %lu\n\n", gates.num_gates);

    label_gates(&gates);

    for (size_t i = 0; i < gates.num_gates; ++i) {
        if (gates.gates[i].group == 1) {
            printf("%s\n", gates.gates[i].type);
        }
    }

    return 0;
}
