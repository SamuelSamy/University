import socket
import struct
import typing

HOST = '127.0.0.1'
PORT = 55555

class UDPClient:

    def __init__(self):

        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            self.address = (HOST, PORT)
        except Exception as error:
            print(error)
            exit(1)


    def run(self):

        while True:
            message = input('Enter message: ')
            self.send_string(self.socket, message, self.address)
            new_message, address = self.recv_string(self.socket)
            print(f'Message from {address}: {new_message}')



    def send_number(
        self, 
        socket: socket.socket, 
        number, 
        number_format: typing.Literal['!i', '!f'],
        address: typing.Tuple[str, int]
    ) -> None:
        socket.sendto(struct.pack(number_format, number), address)


    def send_string(
        self, 
        socket: socket.socket, 
        string: str,
        address: typing.Tuple[str, int]
    ) -> None:
        socket.sendto(string.encode(), address)


    def recv_number(
        self, 
        socket: socket.socket, 
        number_format: typing.Literal['!i', '!f']
    ) -> typing.Tuple[int, typing.Tuple[str, int]]:
        number, address = socket.recvfrom(4)
        number = struct.unpack(number_format, number)[0]
        return (number, address)

    
    def recv_string(
        self, 
        socket: socket.socket
    ) -> typing.Tuple[str, typing.Tuple[str, int]]:
        string, address = socket.recvfrom(4096)
        string = string.decode('utf-8')
        return (string, address)



client = UDPClient()
client.run()