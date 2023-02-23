# The client sends to the server an integer. The server returns the list of divisors for the specified number.

import socket
import struct
import threading

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
            

    @staticmethod
    def get_divisors(n: int):
        divisors = []
        for i in range(1, n + 1):
            if n % i == 0:
                divisors.append(i)
        return divisors


    def worker(self, _socket: socket.socket):
        data = _socket.recv(100)

        if not data:
            return

        data = struct.unpack('!q', data)[0]
        print(f'Got: {data}')

        divisors = self.get_divisors(data)
        _socket.send(str(divisors).encode())
        print(f'Sent {divisors} to client.')

        exit(0)


server = Server()
server.run()