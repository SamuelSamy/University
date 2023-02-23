#include "undirected_graph.h"
#include <string>
#include <exception>
#include <fstream>
#include <stdio.h>
#include <ctime>
#include <random>
#include <queue>
#include <stack>


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
    this->edges = graph.edges;
    this->costs = graph.costs;
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

    return this->edges.at(vertex0).find(vertex1) != this->edges.at(vertex0).end() || this->edges.at(vertex1).find(vertex0) != this->edges.at(vertex1).end();
}

int Graph::count_vertices() const
{
    return (int)this->vertices.size();
}

int Graph::count_edges() const
{
    return (int)this->costs.size();
}

int Graph::get_degree(int vertex) const
{
    if (!this->is_vertex(vertex))
    {
        throw std::exception(NoVertex);
    }

    return (int)this->edges.at(vertex).size();
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

    this->costs[getID(vertex0, vertex1)] = value;
    this->costs[getID(vertex1, vertex0)] = value;
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

    return this->costs.at(getID(vertex0, vertex1));
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

    this->edges[vertex0].insert(vertex1);
    this->edges[vertex1].insert(vertex0);
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

    this->edges[vertex0].erase(vertex1);
    this->edges[vertex1].erase(vertex0);
    this->costs.erase(getID(vertex0, vertex1));
}

void Graph::add_vertex(int vertex)
{
    if (this->is_vertex(vertex))
    {
        throw std::exception(DuplcateVertex);
    }

    this->vertices.insert(vertex);
    this->edges[vertex] = std::set<int>();
}

void Graph::remove_vertex(int vertex)
{
    if (!this->is_vertex(vertex))
    {
        throw std::exception(NoVertex);
    }

    for (auto to : this->edges[vertex])
    {
        this->remove_edge(vertex, to);
        this->remove_edge(to, vertex);
    }

    this->vertices.erase(vertex);
    this->edges.erase(vertex);
}

std::unordered_set<int>::iterator Graph::vertices_begin() const
{
    return this->vertices.begin();
}

std::unordered_set<int>::iterator Graph::vertices_end() const
{
    return this->vertices.end();
}

std::set<int>::iterator Graph::edges_begin(int vertex) const
{
    return this->edges.at(vertex).begin();
}

std::set<int>::iterator Graph::edges_end(int vertex) const
{
    return this->edges.at(vertex).end();
}

std::vector<int> Graph::prim()
{
    struct NodeData {
        int node;
        int w;

        NodeData(int node, int w)
        {
            this->node = node;
            this->w = w;
        }

        NodeData() 
        {
            node = -1;
            w = -1;
        }
    };

    struct Comp {
        bool operator()(NodeData n1, NodeData n2)
        {
            return n1.w > n2.w;
        }
    };

    std::priority_queue<NodeData, std::vector<NodeData>, Comp> pq;
    std::vector<int> key(this->count_vertices() + 100, 0x3f3f3f3f);
    std::vector<int> parent(this->count_vertices() + 100, -1);
    std::vector<bool> visited(this->count_vertices() + 100, false);

    std::vector<std::vector<NodeData>> pEdges;
    pEdges.resize(this->count_vertices() + 5);

    std::unordered_map<std::string, bool> inserted;

    for (int i = 0; i < this->count_vertices(); i++)
    {
        for (int next : this->edges[i])
        {
            if (inserted.find(getID(i, next)) == inserted.end() || inserted.find(getID(next, i)) == inserted.end())
            {
                inserted[getID(i, next)] = true;
                inserted[getID(next, i)] = true;
                pEdges[i].push_back(NodeData{next, this->get_edge_data(i, next)});
                pEdges[next].push_back(NodeData{i, this->get_edge_data(next, i)});

            }
        }
    }

    int src = 0;

    pq.push(NodeData{ src, 0 });
    key[src] = 0;

    while (!pq.empty())
    {
        NodeData nodeData = pq.top();
        pq.pop();

        visited[nodeData.node] = true;

        for (NodeData next : pEdges[nodeData.node])
        {
            if (!visited[next.node] && key[next.node] > next.w)
            {
                key[next.node] = next.w;
                parent[next.node] = nodeData.node;
                pq.push(NodeData{ next.node, next.w });
            }
        }
    }

    int sum = 0;

    for (int i = 0; i < this->count_vertices(); i++)
    {
        sum += key[i];
    }
    
    parent[this->count_vertices()] = sum;
    return parent;
}

Graph::~Graph()
{
}
