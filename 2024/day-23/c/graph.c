#include "graph.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>

Node graph_new_node(char* name)
{
    Node n;
    strcpy(n.name, name);
    n.num_links = 0;
    return n;
}

Node* graph_find_node(Graph* g, char* name)
{
    for (size_t i = 0; i < g->num_nodes; ++i) {
        if (strcmp(g->nodes[i].name, name) == 0) {
            return &g->nodes[i];
        }
    }
    return NULL;
}

inline void graph_add_node(Graph* g, Node node)
{
    if (graph_find_node(g, node.name) != NULL) {
        printf("Node %s already in graph.\n", node.name);
        return;
    }
    g->nodes[g->num_nodes] = node;
    g->num_nodes++;
}

void graph_add_link(Graph* g, char* name1, char* name2)
{
    Node* node1 = graph_find_node(g, name1);
    Node* node2 = graph_find_node(g, name2);

    if (node1 == NULL) {
        Node n = graph_new_node(name1);
        graph_add_node(g, n);
        node1 = graph_find_node(g, name1);
    }

    if (node2 == NULL) {
        Node n = graph_new_node(name2);
        graph_add_node(g, n);
        node2 = graph_find_node(g, name2);
    }

    node1->links[node1->num_links] = node2;
    node1->num_links++;

    node2->links[node2->num_links] = node1;
    node2->num_links++;
}
