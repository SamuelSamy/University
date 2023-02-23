import socket
import struct
import typing
import random

PORT = 55555
IP = '127.0.0.1'


class UDPServer:

    def __init__(self):
        
        
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            self.socket.bind((IP, PORT))
        except socket.error as e:
            print(e)
            exit(1)



    def run(self):
        print('Server is running...')
        self.number = random.randint(0, 2 ** 31)
        print(f'Number is {self.number}')
        self.clients = {}

        while True:
            guess, address = self.recv_number(self.socket, '!i')

            if address not in self.clients:
                self.clients[address] = 0

            self.clients[address] += 1

            if self.number == guess:
                self.winner = address
                break

            elif self.number > guess:
                self.send_string(self.socket, 'H', address)

            elif self.number < guess:
                self.send_string(self.socket, 'S', address)

        print(f'Winner is {self.winner}')
        
        for address, tries in self.clients.items():
            if address == self.winner:
                self.send_string(self.socket, 'G', address)
            else:
                self.send_string(self.socket, 'L', address)
            
            self.send_number(self.socket, tries, '!i', address)



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



server = UDPServer()
server.run()