#pragma once

#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <set>

class Graph
{
    private:

        std::unordered_set<int> vertices;

        std::unordered_map<int, std::set<int>> edges;
        std::unordered_map<std::string, int> costs;


    public:

        Graph();
        Graph(int noOfVertices);
        Graph(const Graph& graph);

        bool is_vertex(int vertex) const;
        bool is_edge(int vertex0, int vertex1) const;
        int count_vertices() const;
        int count_edges() const;
        int get_degree(int vertex) const;

        void set_edge_data(int vertex0, int vertex1, int value);
        int get_edge_data(int vertex0, int vertex1) const;

        void add_edge(int vertex0, int vertex1);
        void remove_edge(int vertex0, int vertex1);

        void add_vertex(int vertex);
        void remove_vertex(int vertex);

        std::unordered_set<int>::iterator vertices_begin() const;
        std::unordered_set<int>::iterator vertices_end() const;

        std::set<int>::iterator edges_begin(int vertex) const;
        std::set<int>::iterator edges_end(int vertex) const;

        std::vector<int> prim();

        ~Graph();

        friend std::ostream& operator<<(std::ostream& os, const Graph& g);
};


inline std::ostream& operator<<(std::ostream& os, const Graph& g)
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