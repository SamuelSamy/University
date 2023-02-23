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
        data = _socket.recv(4096)

        if not data:
            return

        data = data.decode('utf-8')
        print(f'Got: {data}')

        whitespaces = data.count(' ')

        _socket.send(struct.pack('!q', whitespaces))
        print(f'Sent {whitespaces} to client.')

        exit(0)


server = Server()
server.run()