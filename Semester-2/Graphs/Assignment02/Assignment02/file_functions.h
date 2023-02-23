#pragma once

#include "directed_graph.h"

// Reads a graph from the given file and returns it
DirectedGraph readGraphFromFile(std::string filePath);

// Writes the given graph the to given file
void writeGraphToFile(DirectedGraph g, std::string filePath);

// Generates a random graph with `vertices` vertices and `edges` edges
DirectedGraph createRandomGraph(int vertices, int edges);
