#include "file_functions.h"

#include <fstream>
#include <ctime>
#include <random>

#define NoEdge "There is no edge between the specified vertices"
#define DuplicateEdge "Duplicate Edge"
#define DuplcateVertex "Duplicate Vertex"
#define NoVertex "There is no vertex with the specified value"
#define InvalidNumberOfVertices "The number of vertices must be a positive number"
#define InvalidNumberOfEdges "The number of edges must be a positive number and must be less then (noOfVertices * noOfVertices)"
#define NoLoopsAllowed "You can not add a loop edge"

UndirectedGraph readGraphFromFile(std::string filePath)
{
    std::ifstream fin;
    fin.open(filePath);

    int n, m;

    fin >> n >> m;

    UndirectedGraph g{ n };

    int vertex0, vertex1, value;

    for (int i = 0; i < m; i++)
    {
        fin >> vertex0 >> vertex1 >> value;
        g.add_edge(vertex0, vertex1);
        g.set_edge_data(vertex0, vertex1, value);
    }

    fin.close();
    return g;
}


void writeGraphToFile(UndirectedGraph g, std::string filePath)
{
    std::ofstream fout;
    fout.open(filePath);
    fout << g.count_vertices() << ' ' << g.count_edges() << '\n' << g;
    fout.close();
}

UndirectedGraph createRandomGraph(int vertices, int edges)
{
    if (vertices < 0)
    {
        throw std::exception(InvalidNumberOfVertices);
    }

    if (edges < 0 || vertices * vertices < edges)
    {
        throw std::exception(InvalidNumberOfEdges);
    }

    std::random_device dev;
    std::mt19937 rng(dev());
    std::uniform_int_distribution<std::mt19937::result_type> random_cost(0, 200);
    std::uniform_int_distribution<std::mt19937::result_type> random_vertex(0, vertices - 1);

    UndirectedGraph g{ vertices };

    for (int i = 0; i < edges; i++)
    {
        try
        {
            int vertex0 = random_vertex(rng);
            int vertex1 = random_vertex(rng);
            int cost = random_cost(rng);

            g.add_edge(vertex0, vertex1);
            g.set_edge_data(vertex0, vertex1, cost);
        }
        catch (...)
        {
            i--;
        }
    }

    return g;
}