#include <fstream>
#include <iostream>
#include <vector>
#include "undirected_graph.h"
#include <algorithm>

std::vector<int> solution;

void findCycle(Graph& g, int node, std::vector<int>& v, std::vector<bool>& visited)
{
    // we have enough vertices in the vector
    if (v.size() == g.count_vertices())
    {
        // check if there is an edge from the last node to the first one
        if (g.is_edge(node, v[0]))
        {
            solution = v;
        }

        return;
    }

    for (auto it = g.edges_begin(node); it != g.edges_end(node); it++)
    {
        int next = *it;

        if (!visited[next])
        {
            visited[next] = true;
            v.push_back(next);
            
            findCycle(g, next, v, visited);

            visited[next] = false;
            v.erase(find(v.begin(), v.end(), next));

            // if we find a solution there's no point in continuing
            if (!solution.empty())
            {
                return;
            }
        }
    }
}

int main()
{
    
    int n, m;

    std::ifstream fin("data.txt");

    fin >> n >> m;
    
    Graph g{ n };

    for (int i = 0; i < m; i++)
    {
        int x, y, c;
        fin >> x >> y >> c;
        g.add_edge(x, y);
        g.set_edge_data(x, y, c);
    }

    std::vector<int> v;
    std::vector<bool> visited(n + 2, false);

    visited[0] = true;
    v.push_back(0);

    findCycle(g, 0, v, visited);

    if (solution.size() == 0)
    {
        // we have no cycles
        std::cout << "There are no cycles!";
        return 0;
    }

    int totalCost = 0;

    std::cout << "The cycle is: ";

    for (int i = 0; i < solution.size(); i++)
    {
        std::cout << solution[i] << " ";
        totalCost += g.get_edge_data(solution[i], solution[(i + 1) % solution.size()]);
    }

    std::cout << solution[0] << "\nThe total cost is: " << totalCost;

    return 0;
}