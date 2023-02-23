import socket
import struct
import typing
import random
import time


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

        start = 0
        stop = 2 ** 31

        while True:
            number = random.randint(start, stop)

            self.send_number(self.socket, number, '!i', self.address)
            response, _ = self.recv_string(self.socket)

            print(f'Guess: {number}; Response: {response}')

            if response == 'G':
                tries, _ = self.recv_number(self.socket, '!i')
                print(f'Game over! You guessed the number within {tries} tries!')
                break

            elif response == 'H':
                start = number + 1

            elif response == 'S':
                stop = number - 1

            elif response == 'L':
                tries, _ = self.recv_number(self.socket, '!i')
                print(f'You lost after {tries} tries!')
                break
            
            time.sleep(.33)


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