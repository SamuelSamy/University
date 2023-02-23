# The client sends the complete path to a file. 
# The server returns back the length of the file and its content in the case the file exists. 
# When the file does not exist the server returns a length of -1 and no content. 
# The client will store the content in a file with the same name as the input file with the suffix –copy appended (ex: for f.txt => f.txt-copy).

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

        path = data.decode('utf-8')
        print(f'Got: {path}')

        try:
            with open(path, 'r') as file:
                content = file.read()
        
        except:
            _socket.send(struct.pack('!q', -1))
            return

        _socket.send(struct.pack('!q', len(content)))
        _socket.send(content.encode('utf-8'))

        print(f'Sent data to client.')
        exit(0)


server = Server()
server.run()