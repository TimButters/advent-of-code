#include "graph.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

int link_overlap(Node** links1, size_t num_links1, Node** links2, size_t num_links2, char overlap_buffer[100][10])
{
    int num_overlap = 0;
    for (size_t i = 0; i < num_links1; ++i) {
        for (size_t j = 0; j < num_links2; ++j) {
            printf("%s\t%s\t%d\n", links1[i]->name, links2[j]->name, strcmp(links1[i]->name, links2[j]->name));
            if (strcmp(links1[i]->name, links2[j]->name) == 0) {
                strcpy(overlap_buffer[num_overlap], links1[i]->name);
                num_overlap++;
            }
        }
    }
    return num_overlap;
}

void find_groups(Graph* g)
{
    char results[100][20];
    size_t num_results = 0;
    char overlap_buffer[100][10];
    for (size_t i = 0; i < g->num_nodes; ++i) {
        for (size_t j = 0; j < g->nodes[i].num_links; ++j) {
            int num_overlap = link_overlap(g->nodes[i].links, g->nodes[i].num_links,
                    g->nodes[i].links[j]->links, g->nodes[i].links[j]->num_links, overlap_buffer);
            if (num_overlap > 1) {
                for (size_t k = 0; k < num_overlap; ++k) {
                    snprintf(results[num_results], 20, "%s-%s-%s\n",
                            g->nodes[i].name, g->nodes[i].links[j]->name, overlap_buffer[k]);
                    num_results++;
                }
            }
        }
    }

    for (size_t i = 0; i < num_results; ++i) {
        printf("%s\n", results[i]);
    }
}

int main(int argc, char** argv)
{
    if (argc < 2) {
        printf("No input file provided.\n");
        return 1;
    }

    Graph g = load_data(argv[1]);
    find_groups(&g);

    return 0;
}
