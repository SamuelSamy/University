import socket
import struct
import typing
import threading
import os

HOST = '127.0.0.1'
PORT = 55555

class Client:

    def __init__(self):

        self.formats = {
            'int' : '!q',
            'float': '!f',
            'double': '!d'
        }

        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.connect((HOST, PORT))
            print('Connected to server.')
        except Exception as error:
            print(error)
            exit(1)


    def handle_input(self, socket):
        while True:
            length = int(input('Enter length of array:\n'))

            array = []
            for _ in range(length):
                element = float(input('Enter element: '))
                array.append(element)

            self.send_array(socket, array)


    def handle_recieve(self, socket):
        while True:
            last_size = self.recv_number(socket, '!I')

            if last_size == 0:
                print('The server has closed.')
                print(f'The fianl array is: {self.array}')
                os._exit(0)

            self.array = self.recv_array(socket)
            print(f'Received {self.array} from server')

            
    def run(self):
        t = threading.Thread(target = self.handle_input, args = (self.socket,))
        t.start()
        self.handle_recieve(self.socket)

    
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


client = Client()
client.run()