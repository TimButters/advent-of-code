#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_WIRES 350
#define MAX_GATES 250
#define LABEL_LENGTH 10

struct Wire {
    char label[LABEL_LENGTH];
    int value;
};

typedef struct {
    struct Wire wires[MAX_WIRES];
    size_t num_wires;
} WireSet;

typedef struct {
    char input1[LABEL_LENGTH];
    char input2[LABEL_LENGTH];
    char output[LABEL_LENGTH];
    char type[4];
    char label[6];
    int group;
    char dodgy;
} Gate;

typedef struct {
    Gate gates[MAX_GATES];
    size_t num_gates;
} GateSet;

int compare_wires(const void* wire1, const void* wire2)
{
    struct Wire* w1 = (struct Wire*)wire1;
    struct Wire* w2 = (struct Wire*)wire2;
    return strcmp(w1->label, w2->label);
}

int compare_strings(const void* wire1, const void* wire2)
{
    char* w1 = (char*)wire1;
    char* w2 = (char*)wire2;
    return strcmp(w1, w2);
}

WireSet build_zs(char wires[][LABEL_LENGTH], int* wire_values, size_t num_wires)
{
    WireSet wire_set;
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
    gate.group = -1;
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

size_t gate_index_by_inputs(GateSet* gates, char* wire[2], Gate** buffer)
{
    size_t num_found = 0;
    for (size_t i = 0; i < gates->num_gates; ++i) {
        if ((strcmp(gates->gates[i].input1, wire[0]) == 0 && strcmp(gates->gates[i].input2, wire[1]) == 0)
            || (strcmp(gates->gates[i].input2, wire[0]) == 0 && strcmp(gates->gates[i].input1, wire[1]) == 0)) {
            buffer[num_found] = &gates->gates[i];
            num_found++;
        }
    }
    return num_found;
}

void assign_group(Gate* gate, int group)
{
    gate->group = group;
}

void label_gates(GateSet* gates, char* wire, int group)
{
    Gate* buffer[2];
    size_t idx_xor1;
    size_t idx_xor2;
    size_t gates_found = 0;
    size_t num_found;

    char* input_bits[2] = { malloc(4), malloc(4) };
    sprintf(input_bits[0], "x%02d", group);
    sprintf(input_bits[1], "y%02d", group);
    num_found = gate_index_by_inputs(gates, input_bits, buffer);
    for (size_t i = 0; i < num_found; ++i) {
        assign_group(buffer[i], group);
        char label[6];
        sprintf(label, "%s 1", buffer[i]->type);
        strcpy(buffer[i]->label, label);
        gates_found++;
    }
    free(input_bits[0]);
    free(input_bits[1]);
    if (group == 0) {
        return;
    }

    // XOR gate 1
    idx_xor1 = gate_index_by_output(*gates, wire);

    if (strcmp(gates->gates[idx_xor1].type, "XOR") != 0) {
        return;
    }

    assign_group(&gates->gates[idx_xor1], group);
    strcpy(gates->gates[idx_xor1].label, "XOR 1");
    gates_found++;

    // XOR gate 2
    size_t test_idx1 = gate_index_by_output(*gates, gates->gates[idx_xor1].input1);
    size_t test_idx2 = gate_index_by_output(*gates, gates->gates[idx_xor1].input2);

    if (strcmp(gates->gates[test_idx1].type, "XOR") == 0) {
        assign_group(&gates->gates[test_idx1], group);
        strcpy(gates->gates[test_idx1].label, "XOR 2");
        idx_xor2 = test_idx1;
        gates_found++;
    } else if (strcmp(gates->gates[test_idx2].type, "XOR") == 0) {
        assign_group(&gates->gates[test_idx2], group);
        strcpy(gates->gates[test_idx2].label, "XOR 2");
        idx_xor2 = test_idx2;
        gates_found++;
    }

    // AND gate 1
    char* inputs[2] = { gates->gates[idx_xor1].input1, gates->gates[idx_xor1].input2 };
    num_found = gate_index_by_inputs(gates, inputs, buffer);
    char or_input1[5];
    for (int i = 0; i < num_found; ++i) {
        if (strcmp(buffer[i]->type, "AND") == 0) {
            assign_group(buffer[i], group);
            strcpy(buffer[i]->label, "AND 1");
            strcpy(or_input1, buffer[i]->output);
            gates_found++;
        }
    }

    // AND gate 2
    char* inputs2[2] = { gates->gates[idx_xor2].input1, gates->gates[idx_xor2].input2 };
    num_found = gate_index_by_inputs(gates, inputs2, buffer);
    char or_input2[5];
    for (int i = 0; i < num_found; ++i) {
        if (strcmp(buffer[i]->type, "AND") == 0) {
            assign_group(buffer[i], group);
            strcpy(buffer[i]->label, "AND 2");
            strcpy(or_input2, buffer[i]->output);
            gates_found++;
        }
    }

    // OR gate
    char* or_inputs[2] = { or_input1, or_input2 };
    num_found = gate_index_by_inputs(gates, or_inputs, buffer);
    for (int i = 0; i < num_found; ++i) {
        if (strcmp(buffer[i]->type, "OR") == 0) {
            assign_group(buffer[i], group);
            strcpy(buffer[i]->label, "OR");
            gates_found++;
        }
    }
}

void swap_outputs(Gate* gate1, Gate* gate2)
{
    char buffer[6];
    strcpy(buffer, gate1->output);
    strcpy(gate1->output, gate2->output);
    strcpy(gate2->output, buffer);
}

void swap_gates_setup(GateSet* gates, GateSet* new_gates, Gate** orphan_gates, size_t* num_orphans, Gate** dodgy_gates, size_t* num_dodgy)
{
    *num_orphans = 0;
    *num_dodgy = 0;

    new_gates->num_gates = gates->num_gates;
    for (size_t i = 0; i < gates->num_gates; ++i) {
        new_gates->gates[i] = gates->gates[i];
    }

    for (size_t i = 0; i < new_gates->num_gates; ++i) {
        if (new_gates->gates[i].group == -1) {
            orphan_gates[*num_orphans] = &new_gates->gates[i];
            (*num_orphans)++;
        } else if (new_gates->gates[i].dodgy) {
            dodgy_gates[*num_dodgy] = &new_gates->gates[i];
            (*num_dodgy)++;
        }
    }
}

void label_all_gates(GateSet* gates)
{
    char output_bit[4];
    for (int i = 0; i < 45; ++i) {
        sprintf(output_bit, "z%02d", i);
        label_gates(gates, output_bit, i);
    }
}

size_t label_dodgy_gates(GateSet* gates)
{
    for (size_t i = 0; i < gates->num_gates; ++i) {
        gates->gates[i].dodgy = 0;
    }

    size_t num_dodgy = 0;
    for (int group = -1; group < 45; ++group) {
        size_t group_gates = 0;
        size_t group_and = 0;
        size_t group_xor = 0;
        size_t group_or = 0;
        for (size_t i = 0; i < gates->num_gates; ++i) {
            if (gates->gates[i].group == group) {
                group_gates++;
                if (strcmp(gates->gates[i].type, "AND") == 0) {
                    group_and++;
                } else if (strcmp(gates->gates[i].type, "XOR") == 0) {
                    group_xor++;
                } else {
                    group_or++;
                }
            }
        }
        if ((group > 0 && (group_and != 2 || group_xor != 2 || group_or != 1))) {
            num_dodgy++;
            for (int i = 0; i < gates->num_gates; ++i) {
                if (gates->gates[i].group == group) {
                    gates->gates[i].dodgy = 1;
                }
            }
        }
    }

    return num_dodgy;
}

size_t calc_num_healthy_bits(const GateSet* gates)
{
    size_t num_healthy_bits = 0;
    for (size_t g = 0; g < 45; ++g) {
        size_t group_and = 0;
        size_t group_xor = 0;
        size_t group_or = 0;
        for (size_t i = 0; i < gates->num_gates; ++i) {
            if (gates->gates[i].group == g) {
                if (strcmp(gates->gates[i].type, "AND") == 0) {
                    group_and++;
                } else if (strcmp(gates->gates[i].type, "XOR") == 0) {
                    group_xor++;
                } else {
                    group_or++;
                }
            }
        }
        if (group_and == 2 && group_xor == 2 && group_or == 1) {
            num_healthy_bits++;
        }
    }
    return num_healthy_bits;
}

void run_swaps(GateSet* new_gates, GateSet* gates,
    Gate* orphan_gates[], size_t num_orphans,
    Gate* dodgy_gates[], size_t num_dodgy,
    char answer[8][4], size_t* num_answers,
    bool same_set, bool dodgy_set)
{
    Gate** gate_set1 = orphan_gates;
    Gate** gate_set2 = dodgy_gates;
    size_t num_set1 = num_orphans;
    size_t num_set2 = num_dodgy;
    if (same_set) {
        if (dodgy_set) {
            gate_set1 = dodgy_gates;
            num_set1 = num_dodgy;
        } else {
            gate_set2 = orphan_gates;
            num_set2 = num_orphans;
        }
    }

    size_t num_healthy_bits = calc_num_healthy_bits(gates);

    for (size_t i = 0; i < num_set2; ++i) {
        for (size_t j = 0; j < num_set1; ++j) {
            if (gate_set2[i] == gate_set1[j]) {
                continue;
            }
            swap_outputs(gate_set2[i], gate_set1[j]);

            label_all_gates(new_gates);

            size_t new_num_healthy_bits = calc_num_healthy_bits(new_gates);

            if (new_num_healthy_bits > num_healthy_bits) {
                printf("%s swapped with %s\n", gate_set2[i]->output, gate_set1[j]->output);
                strcpy(answer[*num_answers], gate_set2[i]->output);
                strcpy(answer[*(num_answers) + 1], gate_set1[j]->output);
                *num_answers += 2;
                for (size_t m = 0; m < gates->num_gates; ++m) {
                    gates->gates[m] = new_gates->gates[m];
                    if (gates->gates[m].group == gate_set2[i]->group) {
                        gates->gates[m].dodgy = 0;
                    }
                }
                num_healthy_bits = new_num_healthy_bits;
            }
            swap_gates_setup(gates, new_gates, orphan_gates, &num_orphans, dodgy_gates, &num_dodgy);
        }
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

    Gate* orphan_gates[10];
    size_t num_orphans = 0;
    Gate* dodgy_gates[100];
    size_t num_dodgy = 0;
    GateSet new_gates;
    char answer[10][4];
    size_t num_answers = 0;

    label_all_gates(&gates);
    num_dodgy = label_dodgy_gates(&gates);

    swap_gates_setup(&gates, &new_gates, orphan_gates, &num_orphans, dodgy_gates, &num_dodgy);

    printf("Dodgy - Orphan Swaps\n");
    printf("---------------------\n");
    run_swaps(&new_gates, &gates, orphan_gates, num_orphans, dodgy_gates, num_dodgy, answer, &num_answers, false, false);

    printf("\nOrphan - Orphan Swaps\n");
    printf("---------------------\n");
    run_swaps(&new_gates, &gates, orphan_gates, num_orphans, dodgy_gates, num_dodgy, answer, &num_answers, true, false);

    // Relabel dodgy gates now that cross bit swaps have been made.
    num_dodgy = label_dodgy_gates(&gates);

    printf("\nDodgy - Dodgy Swaps\n");
    printf("---------------------\n");
    swap_gates_setup(&gates, &new_gates, orphan_gates, &num_orphans, dodgy_gates, &num_dodgy);
    run_swaps(&new_gates, &gates, orphan_gates, num_orphans, dodgy_gates, num_dodgy, answer, &num_answers, true, true);

    qsort(answer, 8, 4, compare_strings);

    printf("\nPart 2: ");
    for (size_t i = 0; i < num_answers; ++i) {
        if (i > 0) {
            printf(",");
        }
        printf("%s", answer[i]);
    }
    printf("\n");

    return 0;
}
// cgh,frt,pmd,sps,tst,z05,z11,z23
