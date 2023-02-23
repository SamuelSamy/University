import socket
import struct
import threading
import typing

PORT = 55555

class Server:

    def __init__(self):
        
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.bind(('0.0.0.0', PORT))
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


    def send_number(self, socket, number, number_format: typing.Literal['!i', '!f']):
        socket.send(struct.pack(number_format, number))


    def send_string(self, socket, string):
        socket.send(struct.pack(string.encode()))


    def recv_number(self, socket, number_format: typing.Literal['!i', '!f']):
        data = socket.recv(4)
        data = struct.unpack(number_format, data)[0]
        return data

    
    def recv_string(self, socket):
        data = socket.recv(4096)
        data = data.decode('utf-8')
        return data


    def send_array(self, socket, array):
        self.send_number(socket, len(array), '!i')
        for element in array:
            self.send_number(socket, element, '!f')


    def recv_array(self, socket):
        array = []
        length = self.recv_number(socket, '!i')
        for _ in range(length):
            element = self.recv_number(socket, '!f')
            array.append(element)
        return array


server = Server()
server.run()