import socket
import struct
import threading
import typing

PORT = 55555
IP = '0.0.0.0'

class TCPServer:

    def __init__(self):
        self.address = (IP, PORT)
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.bind(self.address)
            self.socket.listen(10)
        except Exception as e:
            print(e)
            exit(1)


    def run(self):
        print('Server is running...')
        while True:
            connection_socket, address = self.socket.accept()
            print(f'Connected {address}')

            threading.Thread(
                target = self.worker,
                args = (connection_socket,)
            ).start()
            


    def worker(self, _socket: socket.socket):
        str = self.recv_string(_socket)
        print(f'Got: {str}')
        
        whitespaces = str.count(' ')
        self.send_number(_socket, whitespaces, 'int')
        print(f'Sent: {whitespaces} to client')

        exit(0)


    def send_number(
        self, 
        socket: socket.socket, 
        number: typing.Union[int, float], 
        number_format: typing.Literal['!i', '!f']
    ):
        socket.send(struct.pack(number_format, number))


    def send_string(
        self, 
        socket: socket.socket, 
        string: str
    ):
        socket.send(struct.pack(string.encode()))


    def recv_number(
        self, 
        socket: socket.socket, 
        number_format: typing.Literal['!i', '!f']
    ) -> typing.Union[int, float]:
        number = socket.recv(4)
        return struct.unpack(number_format, number)[0]
        

    
    def recv_string(
        self, 
        socket: socket.socket
    ) -> str:
        string = socket.recv(4096)
        return string.decode('utf-8')
        


    def send_array(
        self, 
        socket: socket.socket, 
        array: typing.List[float]
    ):
        self.send_number(socket, len(array), '!i')
        for element in array:
            self.send_number(socket, element, '!f')


    def recv_array(
        self, 
        socket: socket.socket
    ) -> typing.List[float]:
        array = []
        length = self.recv_number(socket, '!i')
        for _ in range(length):
            element = self.recv_number(socket, '!f')
            array.append(element)
        return array


server = TCPServer()
server.run()