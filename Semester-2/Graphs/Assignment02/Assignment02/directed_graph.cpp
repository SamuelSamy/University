#pragma warning(disable : 4996)

#include "directed_graph.h"
#include "graph.h"
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

DirectedGraph::DirectedGraph()
{
}

DirectedGraph::DirectedGraph(int noOfVertices)
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

DirectedGraph::DirectedGraph(const DirectedGraph& graph)
{
    this->vertices = graph.vertices;
    this->transpose = graph.transpose;
    this->edges = graph.edges;
    this->costs = graph.costs;
}

bool DirectedGraph::is_vertex(int vertex) const
{
    return this->vertices.find(vertex) != this->vertices.end();
}

bool DirectedGraph::is_edge(int vertex0, int vertex1) const
{
    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
    {
        throw std::exception(NoVertex);
    }

    return this->edges.at(vertex0).find(vertex1) != this->edges.at(vertex0).end();
}

int DirectedGraph::count_vertices() const
{
    return this->vertices.size();
}

int DirectedGraph::count_edges() const
{
    return this->costs.size();
}

int DirectedGraph::get_in_degree(int vertex) const
{
    if (!this->is_vertex(vertex))
    {
        throw std::exception(NoVertex);
    }

    return (int)this->transpose.at(vertex).size();
}

int DirectedGraph::get_out_degree(int vertex) const
{
    if (!this->is_vertex(vertex))
    {
        throw std::exception(NoVertex);
    }

    return (int)this->edges.at(vertex).size();
}


void DirectedGraph::set_edge_data(int vertex0, int vertex1, int value)
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
}

int DirectedGraph::get_edge_data(int vertex0, int vertex1) const
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

void DirectedGraph::add_edge(int vertex0, int vertex1)
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
    this->transpose[vertex1].insert(vertex0);
    this->set_edge_data(vertex0, vertex1, 0);
}

void DirectedGraph::remove_edge(int vertex0, int vertex1)
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
    this->transpose[vertex1].erase(vertex0);
    this->costs.erase(getID(vertex0, vertex1));
}

void DirectedGraph::add_vertex(int vertex)
{
    if (this->is_vertex(vertex))
    {
        throw std::exception(DuplcateVertex);
    }

    this->vertices.insert(vertex);
    this->transpose[vertex] = std::unordered_set<int>();
    this->edges[vertex] = std::unordered_set<int>();
}

void DirectedGraph::remove_vertex(int vertex)
{
    if (!this->is_vertex(vertex))
    {
        throw std::exception(NoVertex);
    }

    for (auto to : this->edges[vertex])
    {
        this->remove_edge(vertex, to);
    }

    for (auto from : this->transpose[vertex])
    {
        this->remove_edge(from, vertex);
    }

    this->vertices.erase(vertex);
    this->transpose.erase(vertex);
    this->edges.erase(vertex);
}


std::unordered_set<int>::iterator DirectedGraph::vertices_begin() const
{
    return this->vertices.begin();
}

std::unordered_set<int>::iterator DirectedGraph::vertices_end() const
{
    return this->vertices.end();
}

std::unordered_set<int>::iterator DirectedGraph::in_begin(int vertex) const
{
    return this->transpose.at(vertex).begin();
}

std::unordered_set<int>::iterator DirectedGraph::in_end(int vertex) const
{
    return this->transpose.at(vertex).end();
}

std::unordered_set<int>::iterator DirectedGraph::out_begin(int vertex) const
{
    return this->edges.at(vertex).begin();
}

std::unordered_set<int>::iterator DirectedGraph::out_end(int vertex) const
{
    return this->edges.at(vertex).end();
}

std::vector<int> DirectedGraph::bfs_transpose(int start, int end)
{
    std::queue<int> q;
    std::vector<int> dist(this->vertices.size(), 0);

    q.push(end);
    dist[end] = 1;

    while (!q.empty())
    {
        int node = q.front();
        q.pop();

        if (node == start)
        {
            break;
        }

        for (int next : this->transpose[node])
        {
            if (dist[next] == 0)
            {
                dist[next] = dist[node] + 1;
                q.push(next);
            }
        }
    }

    if (dist[start] == 0)
    {
        return std::vector<int>();
    }

    std::vector<int> path;
    int node = end;
    

    while (node != start)
    {
        path.push_back(node);

        for (int next : this->transpose[node])
        {
            if (dist[next] == dist[node] + 1)
            {
                node = next;
                break;
            }
        }
    }

    path.push_back(start);
    std::reverse(path.begin(), path.end());
    return path;
}

void DirectedGraph::DFS(int node, std::vector<bool>& visited)
{
    std::stack<int> stk;

    visited[node] = true;
    stk.push(node);


    while (!stk.empty())
    {
        node = stk.top();
        stk.pop();

        for (int next : this->edges[node])
        {
            if (!visited[next])
            {
                visited[next] = true;
                stk.push(next);
            }
        }
    }
}


std::vector<int> DirectedGraph::DFSTranspose(int node, std::vector<bool>& visited)
{
    std::vector<int> component;
    std::stack<int> stk;

    visited[node] = true;
    stk.push(node);


    while (!stk.empty())
    {
        node = stk.top();
        component.push_back(node);
        stk.pop();

        for (int next : this->transpose[node])
        {
            if (!visited[next])
            {
                visited[next] = true;
                stk.push(next);
            }
        }
    }

    return component;
}

std::vector<std::vector<int>> DirectedGraph::strongly_connected_components()
{
    std::vector<std::vector<int>> result;

    std::vector<bool> visited(this->vertices.size(), false);

    std::stack<int> stk;

    for (auto node : this->vertices)
    {
        if (!visited[node])
        {
            DFS(node, visited);
            stk.push(node);
        }
    }

    std::fill(visited.begin(), visited.end(), false);

    while (!stk.empty())
    {
        result.push_back(DFSTranspose(stk.top(), visited));
        stk.pop();
    }

    for (int vertex : this->vertices)
    {
        if (!visited[vertex])
        {
            std::vector<int> v;
            v.push_back(vertex);

            result.push_back(v);
        }
    }

    return result;
}

DirectedGraph::~DirectedGraph()
{

}


