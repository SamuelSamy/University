from graph import Graph, generate_graph, read_from_file, write_to_file

def print_menu():
    print()
    print("1. Generate graph")
    print("2. Get number of vertices")
    print("3. Get number of edges")
    print("4. Add vertex")
    print("5. Remove vertex")
    print("6. Add edge")
    print("7. Remove edge")
    print("8. Set edge cost")
    print("9. Get edge cost")
    print("10. Get in degree")
    print("11. Get out degree")
    print("12. Check if vertex exists")
    print("13. Check if edge exists")
    print("14. Iterate vertices")
    print("15. Iterate inbound edges")
    print("16. Iterate outbound edges")
    print("17. Print graph")
    print("18. Read graph from file")
    print("19. Write graph to file")
    print("20. Copy graph")
    print("0. Exit")
    print()


def main():
    
    g = Graph()

    while True:
        

        print_menu()

        try:
            option = int(input(">> "))

            if option == 0:
                break

            if option == 1:
                vertices = int(input(">> Enter number of vertices: "))
                edges = int(input(">> Enter number of edges: "))
                g = generate_graph(vertices, edges)
                print("Graph successfully generated!")
                continue
        
            if option == 2:
                print(f"The number of vertices is {g.count_vertices()}")
                continue

            if option == 3:
                print(f"The number of edges is {g.count_edges()}")
                continue

            if option == 4:
                new_vertex = int(input(">> Enter the new vertex: "))
                g.add_vertex(new_vertex)
                print(f"Vertex successfully added!")
                continue
        
            if option == 5:
                vertex = int(input(">> Enter the vertex: "))
                g.remove_vertex(vertex)
                print(f"Vertex successfully removed!")
                continue

            if option == 6:
                vertex0 = int(input(">> Vertex0: "))
                vertex1 = int(input(">> Vertex1: "))
                g.add_edge(vertex0, vertex1)
                print(f"Edge successfully added!")
                continue

            if option == 7:
                vertex0 = int(input(">> Vertex0: "))
                vertex1 = int(input(">> Vertex1: "))
                g.remove_edge(vertex0, vertex1)
                print(f"Edge successfully removed!")
                continue

            if option == 8:
                vertex0 = int(input(">> Vertex0: "))
                vertex1 = int(input(">> Vertex1: "))
                cost = int(input(">> Cost: "))
                g.set_edge_data(vertex0, vertex1, cost)
                print(f"Cost successfully added!")
                continue

            if option == 9:
                vertex0 = int(input(">> Vertex0: "))
                vertex1 = int(input(">> Vertex1: "))
                cost = g.get_edge_data(vertex0, vertex1)
                print(f"Cost from vertex {vertex0} to vertex {vertex1} is {cost}")
                continue

            if option == 10:
                vertex = int(input(">> Vertex: "))
                print(f"In degree of vertex {vertex} is {g.get_in_degree(vertex)}")
                continue

            if option == 11:
                vertex = int(input(">> Vertex: "))
                print(f"Out degree of vertex {vertex} is {g.get_out_degree(vertex)}")
                continue

            if option == 12:
                vertex = int(input(">> Vertex: "))

                if g.is_vertex(vertex):
                    print("This vertex exists")
                    continue
                    
                print("This vertex does not exist")
                continue

            if option == 13:
                vertex0 = int(input(">> Vertex0: "))
                vertex1 = int(input(">> Vertex1: "))

                if g.is_edge(vertex0, vertex1):
                    print("This edge exists")
                    continue
                    
                print("This edge does not exist")
                continue

            if option == 14:
                for vertex in g.vertices_iterator():
                    print(vertex, end =' ')    
                print()
                continue

            if option == 15:
                vertex = int(input(">> Vertex: "))

                for y in g.inbound_iterator(vertex):
                    print(y, end = ' ')
                print()
                continue

            if option == 16:
                vertex = int(input(">> Vertex: "))

                for y in g.outbound_iterator(vertex):
                    print(y, end = ' ')
                print()
                continue

            if option == 17:
                print(g)
                continue

            if option == 18:
                path = input(">> Enter path: ")
                g = read_from_file(path)
                print("Graph successfully read!")
                continue

            if option == 19:
                path = input(">> Enter path: ")
                write_to_file(g, path)
                print("Graph successfully written!")
                continue

            if option == 20:
                newG = g.get_copy()
                
                i = 0
                while True:
                    try:
                        newG.remove_vertex(i)
                        break
                    except:
                        i += 1
                        
                print(f"The new graph has {newG.count_vertices()} vertices\nThe old graph has {g.count_vertices()} vertices")
        
        except ValueError as error:
            print(error)


main()