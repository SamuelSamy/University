import socket
import struct
import threading
import typing
import sys
import random
import time

PORT = 1234
IP = '127.0.0.1'

class Client:

    def __init__(self, N):
        self.address = (IP, PORT)
        self.N = N        
        self.lock: threading.Lock = threading.Lock()

    def create_tcp_socket(
        self, 
        address: typing.Tuple[str, int]
    ) -> socket.socket:
        _socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        _socket.connect(address)
        return _socket


    def create_udp_socket(
        self, 
        address: typing.Tuple[str, int]
    ) -> socket.socket:
        _socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        _socket.bind(address)
        return _socket

    def run(self):
        self.finished = False
        print('Client is running...')

        self.tcp_socket = self.create_tcp_socket(self.address)

        self.udp_port = self.recv_number_tcp(self.tcp_socket, '!i')

        self.udp_socket = self.create_udp_socket((IP, self.udp_port))

        t1 = threading.Thread(target=self.tcp_handler)
        t2 = threading.Thread(target=self.udp_handler)

        t1.start()
        t2.start()

        t1.join()
        t2.join()


    def tcp_handler(self):

        for _ in range(25):

            if self.finished:
                break

            x = random.randint(0, self.N)
            y = random.randint(0, self.N)

            string = f'{x};{y}'
            print(f'Sending {string} to server...')
            self.send_number_tcp(self.tcp_socket, len(string), '!i')
            self.send_string_tcp(self.tcp_socket, string)


            sign = self.recv_string_tcp(self.tcp_socket, buffer_size = 1)
            number = self.recv_number_tcp(self.tcp_socket, number_format = '!i')

            if sign == '-':
                number *= -1


            if number == 0:
                print(f'No treasure on ({x},{y})')
            elif number == 1:
                print(f'You found a treasure! ({x}, {y})')
            elif number == 2:
                print(f'This treasure was already found! ({x}, {y})')
            elif number == -1:
                print(f'Invalid position! ({x}, {y})')


            time.sleep(2)


        self.finished = True
        self.tcp_socket.close()
        

    def udp_handler(self):
        while not self.finished:
            string, _ = self.recv_string_udp(self.udp_socket)

            if string == 'exit':
                break
            
            print(string)

            hidden = int(string.split(';')[1].split(' ')[1])
            if hidden == 0:
                break
            

        self.finished = True
        self.udp_socket.close()


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
            raise ConnectionAbortedError('Connection aborted')

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



client = Client(N = 10)
client.run()