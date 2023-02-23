#pragma once

#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>


class DirectedGraph
{
    private:

        std::unordered_set<int> vertices;
        
        std::unordered_map<int, std::unordered_set<int>> edges; // outbound
        std::unordered_map<int, std::unordered_set<int>> transpose;  // inbound
        std::unordered_map<std::string, int> costs;
     

    public:

        // Constructs an empty graph
        DirectedGraph();

        // Constructs a graph with noOfVertices vertices
        DirectedGraph(int noOfVertices);

        // Copy constructor
        DirectedGraph(const DirectedGraph& graph);

        // Returns true if the vertex is in the graph, false otherwise
        bool is_vertex(int vertex) const;

        // Returns true if (vertex0, vertex1) is an edge in the graph, false otherwise
        bool is_edge(int vertex0, int vertex1) const;

        // Returns the number of vertices
        int count_vertices() const;

        // Returns the number of edges
        int count_edges() const;

        // Returns the in degree of a vertex
        int get_in_degree(int vertex) const;

        // Returns the out degree of a vertex
        int get_out_degree(int vertex) const;

        // Sets the cost of the edge (vertex0, vertex1) to value
        void set_edge_data(int vertex0, int vertex1, int value);

        // Gets the cost of the edge (vertex0, vertex1)
        int get_edge_data(int vertex0, int vertex1) const;

        // Adds an edge from vertex0 to vertex1, default cost is 0
        void add_edge(int vertex0, int vertex1);

        // Removes the edge from vertex0 to vertex1
        void remove_edge(int vertex0, int vertex1);

        // Adds the given vertex to the graph
        void add_vertex(int vertex);

        // Removes the given vertex from the graph
        void remove_vertex(int vertex);

        // Returns an iterator to the begining of the vertex set
        std::unordered_set<int>::iterator vertices_begin() const;

        // Returns an iterator to the end of the vertex set
        std::unordered_set<int>::iterator vertices_end() const;

        // Returns an iterator to the begining of the inbound vertices of vertex
        std::unordered_set<int>::iterator in_begin(int vertex) const;

        // Returns an iterator to the end of the inbound vertices of vertex
        std::unordered_set<int>::iterator in_end(int vertex) const;

        // Returns an iterator to the begining of the outbound vertices of vertex
        std::unordered_set<int>::iterator out_begin(int vertex) const;

        // Returns an iterator to the end of the outbound vertiecs of vertex
        std::unordered_set<int>::iterator out_end(int vertex) const;

        // Gets the shortest path on inbound edges from `end` to `start`
        std::vector<int> bfs_transpose(int start, int end);

        // Gets the strongly conected components
        std::vector<std::vector<int>> strongly_connected_components();

        // Destructor of the class
        ~DirectedGraph();

        // Returns an ostream representing the graph
        friend std::ostream& operator<<(std::ostream& os, const DirectedGraph& g);

    private:
        void DFS(int node, std::vector<bool>& visited);
        std::vector<int> DFSTranspose(int node, std::vector<bool>& visited);
};


inline std::ostream& operator<<(std::ostream& os, const DirectedGraph& g)
{
    for (auto vertex_it = g.vertices.begin(); vertex_it != g.vertices.end(); vertex_it++)
    {
        auto vertex = *vertex_it;
        for (auto node_it = g.edges.at(vertex).begin(); node_it != g.edges.at(vertex).end(); node_it++)
        {
            auto node = *node_it;
            os << vertex << ' ' << node << ' ' << g.get_edge_data(vertex, node) << '\n';
        }
    }

    return os;
}