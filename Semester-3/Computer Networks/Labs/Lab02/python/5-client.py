import socket
import struct
import typing

HOST = '127.0.0.1'
PORT = 55555

class Client:

    def __init__(self):

        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.connect((HOST, PORT))
            print('Connected to server.')
        except Exception as error:
            print(error)
            exit(1)


    def run(self):

        while True:
            try:
                number = int(input('Enter a number: '))
            except:
                print('Invalid number.')
                continue

            self.send_number(self.socket, number, number_format = '!i')

            try:
                response = self.recv_string(self.socket)
            except:
                print('Server disconnected.')
                break

            print(response)


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