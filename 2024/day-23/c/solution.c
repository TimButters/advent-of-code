#include <stdio.h>
#include <string.h>
#include "graph.h"

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
        graph_add_link(&g, node1, node2);
    }

    return g;
}

int main(int argc, char** argv)
{
    if (argc < 2) {
        printf("No input file provided.\n");
        return 1;
    }

    Graph g = load_data(argv[1]);
    
    printf("%zu\n", g.num_nodes);

    return 0;
}
