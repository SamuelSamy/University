# The client send to the server two sorted array of chars. The server will merge sort the two arrays and return the result to the client.

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
            


    def worker(self, _socket: socket.socket):
        s0 = _socket.recv(4096)
        s1 = _socket.recv(4096)

        if not s0 or not s1:
            return

        s0 = s0.decode()
        s1 = s1.decode()
        print(f'Got: `{s0}`, `{s1}`')

        result = self.merge(s0, s1)
        _socket.send(str(result).encode('utf-8'))
        print(f'Sent {result} to client.')

        exit(0)
    

    def merge(self, s0, s1):
        result = [None] * (len(s0) + len(s1))

        i, j, k = 0, 0, 0

        while i < len(s0) and j < len(s1):
            if s0[i] < s1[j]:
                result[k] = s0[i]
                i += 1
                k += 1
                continue

            result[k] = s1[j]
            j += 1
            k += 1


        while i < len(s0):
            result[k] = s0[i]
            i += 1
            k += 1

        while j < len(s1):
            result[k] = s1[j]
            j += 1
            k += 1

        return result



server = Server()
server.run()