#pragma once

#include "undirected_graph.h"

// Reads a graph from the given file and returns it
UndirectedGraph readGraphFromFile(std::string filePath);

// Writes the given graph the to given file
void writeGraphToFile(UndirectedGraph g, std::string filePath);

// Generates a random graph with `vertices` vertices and `edges` edges
UndirectedGraph createRandomGraph(int vertices, int edges);
