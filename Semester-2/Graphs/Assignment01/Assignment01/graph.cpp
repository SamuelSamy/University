#pragma warning(disable : 4996)

#include "graph.h"
#include <string>
#include <exception>
#include <fstream>
#include <stdio.h>
#include <ctime>
#include <random>


#define NoEdge "There is no edge between the specified vertices"
#define DuplicateEdge "Duplicate Edge"
#define DuplcateVertex "Duplicate Vertex"
#define NoVertex "There is no vertex with the specified value"
#define InvalidNumberOfVertices "The number of vertices must be a positive number"
#define InvalidNumberOfEdges "The number of edges must be a positive number and must be less then (noOfVertices * noOfVertices)"
#define NoLoopsAllowed "You can not add a loop edge"


std::string getID(int vertex0, int vertex1)
{
    return std::to_string(vertex0) + "-" + std::to_string(vertex1);
}

Graph::Graph()
{
}

Graph::Graph(int noOfVertices)
{
    if (noOfVertices < 0)
    {
        throw std::exception(InvalidNumberOfVertices);
    }

    for (int i = 0; i < noOfVertices; i++)
    {
        this->add_vertex(i);
    }
}

Graph::Graph(const Graph& graph)
{
    this->vertices = graph.vertices;
    this->inbound = graph.inbound;
    this->outbound = graph.outbound;

    this->edges = graph.edges;
}

bool Graph::is_vertex(int vertex) const
{
    return this->vertices.find(vertex) != this->vertices.end();
}

bool Graph::is_edge(int vertex0, int vertex1) const
{
    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
    {
        throw std::exception(NoVertex);
    }

    return this->outbound.at(vertex0).find(vertex1) != this->outbound.at(vertex0).end();
}

int Graph::count_vertices() const
{
    return this->vertices.size();
}

int Graph::count_edges() const
{
    return this->edges.size();
}

int Graph::get_in_degree(int vertex) const
{
    if (!this->is_vertex(vertex))
    {
        throw std::exception(NoVertex);
    }

    return (int)this->inbound.at(vertex).size();
}

int Graph::get_out_degree(int vertex) const
{
    if (!this->is_vertex(vertex))
    {
        throw std::exception(NoVertex);
    }

    return (int)this->outbound.at(vertex).size();
}


void Graph::set_edge_data(int vertex0, int vertex1, int value)
{
    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
    {
        throw std::exception(NoVertex);
    }

    if (!this->is_edge(vertex0, vertex1))
    {
        throw std::exception(NoEdge);
    }

    this->edges[getID(vertex0, vertex1)] = value;
}

int Graph::get_edge_data(int vertex0, int vertex1) const
{
    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
    {
        throw std::exception(NoVertex);
    }

    if (!this->is_edge(vertex0, vertex1))
    {
        throw std::exception(NoEdge);
    }

    return this->edges.at(getID(vertex0, vertex1));
}

void Graph::add_edge(int vertex0, int vertex1)
{
    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
    {
        throw std::exception(NoVertex);
    }

    if (this->is_edge(vertex0, vertex1))
    {
        throw std::exception(DuplicateEdge);
    }

    this->outbound[vertex0].insert(vertex1);
    this->inbound[vertex1].insert(vertex0);
    this->set_edge_data(vertex0, vertex1, 0);
}

void Graph::remove_edge(int vertex0, int vertex1)
{
    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
    {
        throw std::exception(NoVertex);
    }

    if (!this->is_edge(vertex0, vertex1))
    {
        throw std::exception(NoEdge);
    }

    this->outbound[vertex0].erase(vertex1);
    this->inbound[vertex1].erase(vertex0);
    this->edges.erase(getID(vertex0, vertex1));
}

void Graph::add_vertex(int vertex)
{
    if (this->is_vertex(vertex))
    {
        throw std::exception(DuplcateVertex);
    }

    this->vertices.insert(vertex);
    this->inbound[vertex] = std::unordered_set<int>();
    this->outbound[vertex] = std::unordered_set<int>();
}

void Graph::remove_vertex(int vertex)
{
    if (!this->is_vertex(vertex))
    {
        throw std::exception(NoVertex);
    }

    for (auto to : this->outbound[vertex])
    {
        this->remove_edge(vertex, to);
    }

    for (auto from : this->inbound[vertex])
    {
        this->remove_edge(from, vertex);
    }

    this->vertices.erase(vertex);
    this->inbound.erase(vertex);
    this->outbound.erase(vertex);
}


std::unordered_set<int>::iterator Graph::vertices_begin() const
{
    return this->vertices.begin();
}

std::unordered_set<int>::iterator Graph::vertices_end() const
{
    return this->vertices.end();
}

std::unordered_set<int>::iterator Graph::in_begin(int vertex) const
{
    return this->inbound.at(vertex).begin();
}

std::unordered_set<int>::iterator Graph::in_end(int vertex) const
{
    return this->inbound.at(vertex).end();
}

std::unordered_set<int>::iterator Graph::out_begin(int vertex) const
{
    return this->outbound.at(vertex).begin();
}

std::unordered_set<int>::iterator Graph::out_end(int vertex) const
{
    return this->outbound.at(vertex).end();
}

Graph::~Graph()
{

}



Graph readGraphFromFile(std::string filePath)
{
    std::ifstream fin;
    fin.open(filePath);

    int n, m;

    fin >> n >> m;

    Graph g{ n };

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


void writeGraphToFile(Graph g, std::string filePath)
{
    std::ofstream fout;
    fout.open(filePath);
    fout << g.count_vertices() << ' ' << g.count_edges() << '\n' << g;
    fout.close();
}

Graph createRandomGraph(int vertices, int edges)
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

    Graph g{ vertices };
    
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