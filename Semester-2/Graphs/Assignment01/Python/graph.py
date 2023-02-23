import copy
from random import randrange

NoEdge = "There is no edge between the specified vertices"
DuplicateEdge = "Duplicate Edge"
DuplicateVertex = "Duplicate Vertex"
NoVertex  = "There is no vertex with the specified value"
InvalidNumberOfVertices = "The number of vertices must be a positive number"
InvalidNumberOfEdges = "The number of edges must be a positive number and must be less then (noOfVertices * noOfVertices)"

class Graph:

    def __init__(self, n = 0):

        if n < 0:
            raise ValueError(InvalidNumberOfVertices)

        self._vertices = set()
        self._inbound = dict()
        self._outbound = dict()
        self._edges = dict()

        
        for i in range(n):
            self.add_vertex(i)


    def is_vertex(self, vertex):
        return vertex in self._vertices


    def is_edge(self, vertex0, vertex1):

        if vertex0 not in self._vertices or vertex1 not in self._vertices:
            raise ValueError(NoVertex)

        return (vertex0, vertex1) in self._edges.keys()


    def count_vertices(self):
        return len(self._vertices)


    def count_edges(self):
        return len(self._edges)


    def get_in_degree(self, vertex):
        if vertex not in self._vertices:
            raise ValueError(NoVertex)

        return len(self._inbound[vertex])


    def get_out_degree(self, vertex):
        if vertex not in self._vertices:
            raise ValueError(NoVertex)

        return len(self._outbound[vertex])


    def set_edge_data(self, vertex0, vertex1, value):
        if vertex0 not in self._vertices or vertex1 not in self._vertices:
            raise ValueError(NoVertex)

        if (vertex0, vertex1) not in self._edges:
            raise ValueError(NoEdge)

        self._edges[(vertex0, vertex1)] = value


    def get_edge_data(self, vertex0, vertex1):
        if vertex0 not in self._vertices or vertex1 not in self._vertices:
            raise ValueError(NoVertex)

        if (vertex0, vertex1) not in self._edges:
            raise ValueError(NoEdge)

        return self._edges[(vertex0, vertex1)]


    def add_edge(self, vertex0, vertex1):
        if vertex0 not in self._vertices or vertex1 not in self._vertices:
            raise ValueError(NoVertex)

        if (vertex0, vertex1) in self._edges:
            raise ValueError(DuplicateEdge)

        self._outbound[vertex0].add(vertex1)
        self._inbound[vertex1].add(vertex0)
        self._edges[(vertex0, vertex1)] = 0


    def remove_edge(self, vertex0, vertex1):
        if vertex0 not in self._vertices or vertex1 not in self._vertices:
            raise ValueError(NoVertex)

        if (vertex0, vertex1) not in self._edges:
            raise ValueError(NoEdge)

        self._outbound[vertex0].remove(vertex1)
        self._inbound[vertex1].remove(vertex0)
        del self._edges[(vertex0, vertex1)]


    def add_vertex(self, vertex):
        if vertex in self._vertices:
            raise ValueError(DuplicateVertex)

        self._vertices.add(vertex)
        self._inbound[vertex] = set()
        self._outbound[vertex] = set()


    def remove_vertex(self, vertex):
        if vertex not in self._vertices:
            raise ValueError(NoVertex)

        remove = []

        for neighbor in self._outbound[vertex]:
            remove.append((vertex, neighbor))

        for neighbor in self._inbound[vertex]:
            remove.append((neighbor, vertex))
        
        for edge in remove:
            self.remove_edge(edge[0], edge[1])

        
        del self._inbound[vertex]
        del self._outbound[vertex]
        self._vertices.remove(vertex)


    def vertices_iterator(self):
        for vertex in self._vertices:
            yield vertex


    def inbound_iterator(self, vertex):
        if vertex not in self._vertices:
            return ValueError(NoVertex)

        for n in self._inbound[vertex]:
            yield n


    def outbound_iterator(self, vertex):
        if vertex not in self._vertices:
            return ValueError(NoVertex)

        for n in self._outbound[vertex]:
            yield n


    def get_copy(self):
        return copy.deepcopy(self)


    def __str__(self):

        string = ""

        for vertex in self._vertices:
            for y in self._outbound[vertex]:
                string += f"{vertex} {y} {self.get_edge_data(vertex, y)}\n"

        return string


def read_from_file(path):

    with open(path, "r") as file:
        lines = file.readlines()
        line = lines[0].split()
        vertices = int(line[0])
        edges = int(line[1])
       
        g = Graph(vertices)

        for i in range(1, edges + 1):
            line = lines[i].split()
            vertex0 = int(line[0])
            vertex1 = int(line[1])
            cost = int(line[2])
            g.add_edge(vertex0, vertex1)
            g.set_edge_data(vertex0, vertex1, cost)

    return g

def write_to_file(graph: Graph, path):
    with open(path, "w") as file:
        file.write(f"{graph.count_vertices()} {graph.count_edges()}\n")

        for x in graph.vertices_iterator():
            for y in graph.outbound_iterator(x):
                file.write(f"{x} {y} {graph.get_edge_data(x, y)}\n")


def generate_graph(n, m):

    if m < 0 or n * n < m:
        raise ValueError(InvalidNumberOfEdges)

    g = Graph(n)

    i = 0
    while i < m:
        x = randrange(n)
        y = randrange(n)
        cost = randrange(200)

        try:
            g.add_edge(x, y)
            g.set_edge_data(x, y, cost)
        except:
            i -= 1

        i += 1

    return g