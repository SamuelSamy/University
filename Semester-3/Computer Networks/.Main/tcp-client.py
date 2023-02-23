import socket
import struct
import typing

HOST = '127.0.0.1'
PORT = 55555

class Client:

    def __init__(self):
        self.address = (HOST, PORT)
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.connect(self.address)
            print('Connected to server.')
        except Exception as error:
            print(error)
            exit(1)


    def run(self):
        string = input('Enter string: ')
        self.send_string(self.socket, string)
        whitespaces = self.recv_number(self.socket, '!i')
        print(f'Got {whitespaces} from server.')


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


client = Client()
client.run()