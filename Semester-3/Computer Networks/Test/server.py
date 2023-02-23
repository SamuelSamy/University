import socket
import struct
import threading
import typing
import select
import random
import time

PORT = 1234
IP = '127.0.0.1'

class Server:

    def __init__(self, N, treasures):
        self.address = (IP, PORT)
        self.lock = threading.Lock()
        self.N = N
        self.treasures = treasures
        self.finished = False

        self.board = []
        for _ in range(self.N):
            self.board.append([0] * self.N)

        for i in range(self.N * 2):
            
            random_x = random.randint(0, self.N - 1)
            random_y = random.randint(0, self.N - 1)

            while self.board[random_x][random_y] != 0:
                random_x = random.randint(0, self.N - 1)
                random_y = random.randint(0, self.N - 1)

            self.board[random_x][random_y] = 1


    def print_board(self):
        self.lock.acquire()
        for line in self.board:
            print(line)
        self.lock.release()

    def create_tcp_socket(
        self, 
        address: typing.Tuple[str, int]
    ) -> socket.socket:
        _socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        _socket.bind(address)
        _socket.listen(10)
        return _socket


    def create_udp_socket(
        self, 
        address: typing.Tuple[str, int]
    ) -> socket.socket:
        _socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        _socket.bind(address)
        return _socket


    def run(self):
        print('Server is running...')

        self.tcp_socket = self.create_tcp_socket(self.address)
        self.udp_socket = self.create_udp_socket(self.address)

        self.clients = {}
        self.sockets: typing.List[socket.socket] = []

        t1 = threading.Thread(target = self.tcp_handler)
        t2 = threading.Thread(target = self.udp_handler)
        t3 = threading.Thread(target = self.printer)

        t1.start()
        t2.start()
        t3.start()



    def printer(self):
        while not self.finished:
            self.print_board()
            print()
            time.sleep(10)


    def tcp_handler(self):
        while not self.finished:

            try:
                comm_socket, address = self.tcp_socket.accept()
            except ConnectionAbortedError:
                print('Server closed the conenction')
                self.finished = True
                print('Fianl board')
                self.print_board()
                return

            self.lock.acquire()
            self.clients[comm_socket] = address
            self.sockets.append(comm_socket)
            self.lock.release()

            t = threading.Thread(target = self.tcp_communication_handler, args = (comm_socket,))
            t.start()

    
    def tcp_communication_handler(self, _socket):

        self.send_number_tcp(_socket, self.clients[_socket][1], '!i')

        while not self.finished:
            try:
                data_len = self.recv_number_tcp(_socket, '!i')
                data = self.recv_string_tcp(_socket, data_len)

                point = data.split(';')
                if len(point) != 2 or not point[0].isnumeric() or not point[1].isnumeric():
                    self.send_string_tcp(_socket, '-')
                    self.send_number_tcp(_socket, number = 1, number_format = '!i')
                    continue

                x = int(point[0])
                y = int(point[1])

                if x < 0 or x >= self.N or y < 0 or y >= self.N:
                    self.send_string_tcp(_socket, '-')
                    self.send_number_tcp(_socket, number = 1, number_format = '!i')
                    continue
                
                self.lock.acquire()
                
                if self.board[x][y] == 0:
                    self.send_string_tcp(_socket, '+')
                    self.send_number_tcp(_socket, number = 0, number_format = '!i')

                elif self.board[x][y] == 1:
                    self.send_string_tcp(_socket, '+')
                    self.send_number_tcp(_socket, number = 1, number_format = '!i')
                    
                    print(f'{self.clients[_socket]} found the treasure ({x}, {y})')

                    self.board[x][y] = 2

                elif self.board[x][y] == 2:
                    self.send_string_tcp(_socket, '+')
                    self.send_number_tcp(_socket, number = 2, number_format = '!i')

                self.lock.release()
                
            except ConnectionAbortedError:
                self.lock.acquire()
                address = self.clients[_socket]
                print(f'Client {address} discontected.')
                self.send_string_udp(self.udp_socket, f'exit', address)
                self.clients.pop(_socket)
                self.sockets.remove(_socket)
                self.lock.release()
                break



    def udp_handler(self):
        
        while not self.finished:
            while True:
                self.lock.acquire()
                
                string_board = str(self.board)
                discovered = string_board.count('2')
                hidden = string_board.count('1')

                for client in self.clients.values():
                    string = f'{discovered} discovered treasures; {hidden} hidden treasures'
                    self.send_string_udp(self.udp_socket, string, client)
                
                self.lock.release()

                if hidden == 0:
                    self.finished = True
                    break
                
                time.sleep(2)


        
    

    def send_number_tcp(
        self, 
        socket: socket.socket, 
        number: typing.Union[int, float], 
        number_format: typing.Literal['!i', '!f']
    ):
        socket.send(struct.pack(number_format, number))


    def send_string_tcp(
        self, 
        socket: socket.socket, 
        string: str
    ):
        socket.send(string.encode())


    def recv_number_tcp(
        self, 
        socket: socket.socket, 
        number_format: typing.Literal['!i', '!f']
    ) -> typing.Union[int, float]:
        number = socket.recv(4)

        if not number:
            raise ConnectionAbortedError
            
        return struct.unpack(number_format, number)[0]
        

    
    def recv_string_tcp(
        self, 
        socket: socket.socket,
        buffer_size: int = 4096
    ) -> str:
        string = socket.recv(buffer_size)

        if not string:
            raise ConnectionAbortedError

        return string.decode('utf-8')
        


    def send_array_tcp(
        self, 
        socket: socket.socket, 
        array: typing.List[float]
    ):
        self.send_number(socket, len(array), '!i')
        for element in array:
            self.send_number(socket, element, '!f')


    def recv_array_tcp(
        self, 
        socket: socket.socket
    ) -> typing.List[float]:
        array = []
        length = self.recv_number(socket, '!i')
        for _ in range(length):
            element = self.recv_number(socket, '!f')
            array.append(element)
        return array
    

    def send_number_udp(
        self, 
        socket: socket.socket, 
        number, 
        number_format: typing.Literal['!i', '!f'],
        address: typing.Tuple[str, int]
    ) -> None:
        socket.sendto(struct.pack(number_format, number), address)


    def send_string_udp(
        self, 
        socket: socket.socket, 
        string: str,
        address: typing.Tuple[str, int]
    ) -> None:
        socket.sendto(string.encode(), address)


    def recv_number_udp(
        self, 
        socket: socket.socket, 
        number_format: typing.Literal['!i', '!f']
    ) -> typing.Tuple[int, typing.Tuple[str, int]]:
        number, address = socket.recvfrom(4)
        number = struct.unpack(number_format, number)[0]
        return (number, address)

    
    def recv_string_udp(
        self, 
        socket: socket.socket
    ) -> typing.Tuple[str, typing.Tuple[str, int]]:
        string, address = socket.recvfrom(4096)
        string = string.decode('utf-8')
        return (string, address)


N = 10
treasures = N * 4
server = Server(N, treasures)
server.run()