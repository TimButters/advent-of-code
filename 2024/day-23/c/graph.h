#ifndef GRAPH_H
#define GRAPH_H

#include <stddef.h>
#include <string.h>

#define MAX_NODES 10000
#define MAX_LINKS 100
#define MAX_NAME_LENGTH 10

typedef struct Node {
    char name[MAX_NAME_LENGTH];
    struct Node* links[MAX_LINKS];
    size_t num_links;
} Node;

typedef struct Graph {
    Node nodes[MAX_NODES];
    size_t num_nodes;
} Graph;

Node graph_new_node(char* name);

Node* graph_find_node(Graph* g, char* name);

void graph_add_node(Graph* g, Node node);

void graph_add_link(Graph* g, char* name1, char* name2);

#endif
