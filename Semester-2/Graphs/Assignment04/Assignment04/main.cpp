#include <iostream>
#include "undirected_graph.h"
#include "file_functions.h"
#include <string>


void printOptions();
int readInteger(std::string message);
bool isValidOption(int option);
void handleOption(int option, UndirectedGraph& g);

int main()
{
    UndirectedGraph g = UndirectedGraph();

    int option = 0;

    do
    {
        printOptions();

        option = readInteger("Enter an option: ");
        if (!isValidOption(option))
        {
            std::cout << "Invalid option!\n";
            continue;
        }

        //try
        {
            handleOption(option, g);
        }
        /*catch (std::exception error)
        {
            std::cout << error.what();
        }*/

        std::cout << "\n\n";

    } while (option != 0);
}

void printOptions()
{
    std::cout << " 1. Generate graph\n";
    std::cout << " 2. Get number of vertices\n";
    std::cout << " 3. Get number of edges\n";
    std::cout << " 4. Add vertex\n";
    std::cout << " 5. Remove vertex\n";
    std::cout << " 6. Add edge\n";
    std::cout << " 7. Remove edge\n";
    std::cout << " 8. Set edge cost\n";
    std::cout << " 9. Get edge cost\n";
    std::cout << "10. Check if vertex exists\n";
    std::cout << "11. Check if edge exists\n";
    std::cout << "12. Iterate vertices\n";
    std::cout << "13. Print graph\n";
    std::cout << "14. Read graph from file\n";
    std::cout << "15. Write graph to file\n";
    std::cout << "16. Minimal Spanning Tree\n";
    std::cout << " 0. Exit\n\n";
}

int readInteger(std::string message)
{
    int retVal = 0;
    int number = -1;

    std::cout << message << '\n';
    std::cin >> number;

    while (std::cin.fail())
    {
        std::cout << "Error reading number!\n";
        std::cout << message << '\n';
        std::cin.clear();
        std::cin.ignore(256, '\n');
        std::cin >> number;
    }

    return number;
}

bool isValidOption(int option)
{
    return (option >= 0 && option <= 16);
}

void handleOption(int option, UndirectedGraph& g)
{
    if (option == 1)
    {
        //std::cout << "1. Generate graph\n";
        int vertices = readInteger("Enter number of vertices: ");
        int edges = readInteger("Enter number of edges: ");
        g = createRandomGraph(vertices, edges);
        std::cout << "Graph successfully generated!";
        return;
    }

    if (option == 2)
    {
        //std::cout << "2. Get number of vertices\n";
        std::cout << "Number of vertices: " << g.count_vertices();
        return;
    }

    if (option == 3)
    {
        //std::cout << "3. Get number of edges\n";
        std::cout << "Number of edges: " << g.count_edges();
        return;
    }

    if (option == 4)
    {
        //std::cout << "4. Add vertex\n";
        int new_vertex = readInteger("Enter vertex: ");
        g.add_vertex(new_vertex);
        std::cout << "Vertex successfully added!";
        return;
    }

    if (option == 5)
    {
        //std::cout << "5. Remove vertex\n";
        int vertex = readInteger("Enter vertex: ");
        g.remove_vertex(vertex);
        std::cout << "Vertex successfully removed!";
        return;
    }

    if (option == 6)
    {
        //std::cout << "6. Add edge\n";
        int vertex0 = readInteger("Vertex0: ");
        int vertex1 = readInteger("Vertex1: ");
        g.add_edge(vertex0, vertex1);
        std::cout << "Edge successfully added!";
        return;
    }

    if (option == 7)
    {
        //std::cout << "7. Remove edge\n";
        int vertex0 = readInteger("Vertex0: ");
        int vertex1 = readInteger("Vertex1: ");
        g.remove_edge(vertex0, vertex1);
        std::cout << "Edge successfully removed!";
        return;
    }

    if (option == 8)
    {
        //std::cout << "8. Set edge cost\n";
        int vertex0 = readInteger("Vertex0: ");
        int vertex1 = readInteger("Vertex1: ");
        int cost = readInteger("Cost: ");
        g.set_edge_data(vertex0, vertex1, cost);
        std::cout << "Cost successfully set!";
        return;
    }

    if (option == 9)
    {
        //std::cout << "9. Get edge cost\n";
        int vertex0 = readInteger("Vertex0: ");
        int vertex1 = readInteger("Vertex1: ");
        int cost = g.get_edge_data(vertex0, vertex1);
        std::cout << "Cost from vertex " << vertex0 << " to vertex " << vertex1 << " is " << cost;
        return;
    }

    if (option == 10)
    {
        //std::cout << "10. Check if vertex exists\n";
        int vertex = readInteger("Vertex: ");
        bool exists = g.is_vertex(vertex);

        if (exists)
        {
            std::cout << "This vertex exists!";
            return;
        }

        std::cout << "This vertex does not exist!";
        return;
    }

    if (option == 11)
    {
        //std::cout << "11. Check if edge exists\n";
        int vertex0 = readInteger("Vertex0: ");
        int vertex1 = readInteger("Vertex1: ");
        bool exists = g.is_edge(vertex0, vertex1);

        if (exists)
        {
            std::cout << "This edge exists!";
            return;
        }
        std::cout << "This edge does not exist!";
        return;
    }

    if (option == 12)
    {
        //std::cout << "12. Iterate vertices\n";
        for (auto it = g.vertices_begin(); it != g.vertices_end(); it++)
        {
            std::cout << *it << ' ';
        }

        return;
    }

    if (option == 13)
    {
        //std::cout << "13. Print graph\n";
        std::cout << g;
        return;
    }

    if (option == 14)
    {
        //std::cout << "14. Read graph from file\n";
        std::string path;
        std::cout << "Path: ";
        std::cin >> path;
        g = readGraphFromFile(path);
        std::cout << "Graph successfully read!";
        return;
    }

    if (option == 15)
    {
        //std::cout << "15. Write graph to file\n";
        std::string path;
        std::cout << "Path: ";
        std::cin >> path;
        writeGraphToFile(g, path);
        std::cout << "Graph successfully written!";
        return;
    }

    if (option == 16)
    {
        std::vector<int> parents = g.prim();
        std::cout << "Total cost: " << parents[g.count_vertices()] << "\nParent's array:\n";

        for (int i = 0; i < g.count_vertices(); i++)
        {
            std::cout << i << " " << parents[i] << "\n";
        }
        return;
    }
}
