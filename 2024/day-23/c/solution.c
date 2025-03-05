#include "graph.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int strcompare(const void* str1, const void* str2)
{
    char* s1 = (char*)str1;
    char* s2 = (char*)str2;
    return strcmp(s1, s2);
}

Graph load_data(char* filename)
{
    FILE* fptr = fopen(filename, "r");
    if (fptr == NULL) {
        printf("Could not open file %s\n", filename);
    }
    char* buffer = NULL;
    size_t len = 0;

    char* pch;
    char node1[5];
    char node2[5];

    Graph g;

    while (getline(&buffer, &len, fptr) != -1) {
        pch = strtok(buffer, "-");
        strcpy(node1, pch);
        pch = strtok(NULL, "-");
        strcpy(node2, pch);
        node2[strcspn(node2, "\n")] = 0;
        graph_add_link(&g, node1, node2);
    }

    return g;
}

int link_overlap(Node** links1, size_t num_links1, Node** links2, size_t num_links2, char overlap_buffer[][20])
{
    int num_overlap = 0;
    for (size_t i = 0; i < num_links1; ++i) {
        for (size_t j = 0; j < num_links2; ++j) {
            if (strcmp(links1[i]->name, links2[j]->name) == 0) {
                strcpy(overlap_buffer[num_overlap], links1[i]->name);
                num_overlap++;
            }
        }
    }
    return num_overlap;
}

int find_groups(Graph* g)
{
    char (*results)[20] = malloc(sizeof(char[10000][20]));
    size_t num_results = 0;
    char (*overlap_buffer)[20] = malloc(sizeof(char[10000][20]));
    char res[3][20];
    for (size_t i = 0; i < g->num_nodes; ++i) {
        for (size_t j = 0; j < g->nodes[i].num_links; ++j) {
            int num_overlap = link_overlap(g->nodes[i].links, g->nodes[i].num_links,
                g->nodes[i].links[j]->links, g->nodes[i].links[j]->num_links, overlap_buffer);
            if (num_overlap > 0) {
                for (size_t k = 0; k < num_overlap; ++k) {
                    strcpy(res[0], g->nodes[i].name);
                    strcpy(res[1], g->nodes[i].links[j]->name);
                    strcpy(res[2], overlap_buffer[k]);
                    if (res[0][0] == 't' || res[1][0] == 't' || res[2][0] == 't') {
                        qsort(res, 3, sizeof res[0], strcompare);
                        snprintf(results[num_results], 20, "%s-%s-%s", res[0], res[1], res[2]);
                        num_results++;
                    }
                }
            }
        }
    }

    qsort(results, num_results, sizeof results[0], strcompare);

    int num_groups = 0;
    char last_group[20];
    for (size_t i = 0; i < num_results; ++i) {
        if (strcmp(results[i], last_group) != 0) {
            num_groups++;
        }
        strcpy(last_group, results[i]);
    }
    free(results);
    free(overlap_buffer);

    return num_groups;
}

int main(int argc, char** argv)
{
    if (argc < 2) {
        printf("No input file provided.\n");
        return 1;
    }

    Graph g = load_data(argv[1]);
    int num_groups = find_groups(&g);

    printf("Part 1: %d\n", num_groups);

    return 0;
}
