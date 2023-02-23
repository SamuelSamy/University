import socket
import struct
import threading
import typing
import random


PORT = 55555

class Server:

    def __init__(self):
        
        self.clients: typing.Dict[socket.socket, int] = {}


    def run(self):
        print('Server is running...')


        while True:
            self.number = random.randint(1, 200)
            print(f'Number: {self.number}')

            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.bind(('0.0.0.0', PORT))
            self.socket.listen(10)

            while True:

                try:
                    connection_socket, address = self.socket.accept()
                except:
                    print('Game Over.')
                    break
            
                print(f'Connected {address}')

                self.clients[connection_socket] = 0

                threading.Thread(
                    target = self.worker,
                    args = (connection_socket,)
                ).start()
                


    def worker(self, _socket: socket.socket):
        
        while True:
            
            try:
                number = self.recv_number(_socket, number_format = '!i')
            except ConnectionAbortedError:
                print('Client disconnected.')
                break

            
            if number == self.number:
                self.send_string(_socket, f'You win - within {self.clients[_socket] + 1} tries')
                
                for client in self.clients.keys():
                    if client != _socket:
                        self.send_string(client, f'You lose - after {self.clients[client]} tries')
                        client.close()

                
                self.clients = {}
                self.socket.close()
                break
            elif number > self.number:
                self.send_string(_socket, 'smaller')
            else:
                self.send_string(_socket, 'larger')

            self.clients[_socket] += 1

        exit(0)


    def send_number(self, socket, number, number_format: typing.Literal['!i', '!f']):
        socket.send(struct.pack(number_format, number))


    def send_string(self, socket, string):
        socket.send(string.encode())


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