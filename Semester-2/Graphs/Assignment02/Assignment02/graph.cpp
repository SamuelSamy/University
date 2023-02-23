//#include "graph.h"
//
//#pragma warning(disable : 4996)
//
//#include "directed_graph.h"
//#include "graph.h"
//#include <string>
//#include <exception>
//#include <fstream>
//#include <stdio.h>
//#include <ctime>
//#include <random>
//#include <queue>
//
//#define NoEdge "There is no edge between the specified vertices"
//#define DuplicateEdge "Duplicate Edge"
//#define DuplcateVertex "Duplicate Vertex"
//#define NoVertex "There is no vertex with the specified value"
//#define InvalidNumberOfVertices "The number of vertices must be a positive number"
//#define InvalidNumberOfEdges "The number of edges must be a positive number and must be less then (noOfVertices * noOfVertices)"
//#define NoLoopsAllowed "You can not add a loop edge"
//
//
//
//Graph::Graph()
//{
//}
//
//Graph::Graph(int noOfVertices)
//{
//    if (noOfVertices < 0)
//    {
//        throw std::exception(InvalidNumberOfVertices);
//    }
//
//    for (int i = 0; i < noOfVertices; i++)
//    {
//        this->add_vertex(i);
//    }
//}
//
//Graph::Graph(const Graph& graph)
//{
//    this->vertices = graph.vertices;
//    this->edges = graph.edges;
//    this->costs = graph.costs;
//}
//
//bool Graph::is_vertex(int vertex) const
//{
//    return this->vertices.find(vertex) != this->vertices.end();
//}
//
//bool Graph::is_edge(int vertex0, int vertex1) const
//{
//    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
//    {
//        throw std::exception(NoVertex);
//    }
//
//    return this->edges.at(vertex0).find(vertex1) != this->edges.at(vertex0).end();
//}
//
//int Graph::count_vertices() const
//{
//    return this->vertices.size();
//}
//
//int Graph::count_edges() const
//{
//    return this->costs.size();
//}
//
//int Graph::get_degree(int vertex) const
//{
//    if (!this->is_vertex(vertex))
//    {
//        throw std::exception(NoVertex);
//    }
//
//    return (int)this->edges.at(vertex).size();
//}
//
//
//void Graph::set_edge_data(int vertex0, int vertex1, int value)
//{
//    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
//    {
//        throw std::exception(NoVertex);
//    }
//
//    if (!this->is_edge(vertex0, vertex1))
//    {
//        throw std::exception(NoEdge);
//    }
//
//    this->costs[getID(vertex0, vertex1)] = value;
//}
//
//int Graph::get_edge_data(int vertex0, int vertex1) const
//{
//    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
//    {
//        throw std::exception(NoVertex);
//    }
//
//    if (!this->is_edge(vertex0, vertex1))
//    {
//        throw std::exception(NoEdge);
//    }
//
//    return this->costs.at(getID(vertex0, vertex1));
//}
//
//void Graph::add_edge(int vertex0, int vertex1)
//{
//    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
//    {
//        throw std::exception(NoVertex);
//    }
//
//    if (this->is_edge(vertex0, vertex1))
//    {
//        throw std::exception(DuplicateEdge);
//    }
//
//    this->edges[vertex0].insert(vertex1);
//    this->edges[vertex1].insert(vertex0);
//    this->set_edge_data(vertex0, vertex1, 0);
//    this->set_edge_data(vertex1, vertex0, 0);
//}
//
//void Graph::remove_edge(int vertex0, int vertex1)
//{
//    if (!this->is_vertex(vertex0) || !this->is_vertex(vertex1))
//    {
//        throw std::exception(NoVertex);
//    }
//
//    if (!this->is_edge(vertex0, vertex1))
//    {
//        throw std::exception(NoEdge);
//    }
//
//    this->edges[vertex0].erase(vertex1);
//    this->edges[vertex1].erase(vertex0);
//    this->costs.erase(getID(vertex0, vertex1));
//    this->costs.erase(getID(vertex1, vertex0));
//}
//
//void Graph::add_vertex(int vertex)
//{
//    if (this->is_vertex(vertex))
//    {
//        throw std::exception(DuplcateVertex);
//    }
//
//    this->vertices.insert(vertex);
//    this->edges[vertex] = std::unordered_set<int>();
//}
//
//void Graph::remove_vertex(int vertex)
//{
//    if (!this->is_vertex(vertex))
//    {
//        throw std::exception(NoVertex);
//    }
//
//    for (auto to : this->edges[vertex])
//    {
//        this->remove_edge(vertex, to);
//        this->remove_edge(to, vertex);
//    }
//
//    this->vertices.erase(vertex);
//    this->edges.erase(vertex);
//}
//
//
//std::unordered_set<int>::iterator Graph::vertices_begin() const
//{
//    return this->vertices.begin();
//}
//
//std::unordered_set<int>::iterator Graph::vertices_end() const
//{
//    return this->vertices.end();
//}
//
//std::unordered_set<int>::iterator Graph::edges_begin(int vertex) const
//{
//    return this->edges.at(vertex).begin();
//}
//
//std::unordered_set<int>::iterator Graph::edges_end(int vertex) const
//{
//    return this->edges.at(vertex).end();
//}
//
//Graph::~Graph()
//{
//
//}
//
//
