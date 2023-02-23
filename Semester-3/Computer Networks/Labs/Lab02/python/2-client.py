import socket
import struct
import sys


HOST = '127.0.0.1'
PORT = 55555

class Client:

    def __init__(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)


    def run(self):
        try:
            self.socket.connect((HOST, PORT))
        except Exception as error:
            print(error)
            exit(1)

        print('Connected to server.')

        paths = [
            'D:\\...Uni\\Sem3\\Retele\\Lab2\\python\\file.txt',
            'D:\\...Uni\\Sem3\\Retele\\Lab2\\python\\no_file.txt',
        ]

        path = paths[int(sys.argv[1])]
        self.socket.send(path.encode())

        length = self.socket.recv(8)
        length = struct.unpack('!q', length)[0]

        if length == -1:
            print('File not found.')
            exit(1)

        content = self.socket.recv(length)

        with open(f'{path}-copy', 'w') as file:
            file.write(content.decode())

        print(f'Got data from server.')


if len(sys.argv) != 2:
    print('Invalid number of arguments.')
    exit(1)

if sys.argv[1] not in ['0', '1']:
    print('Invalid argument.')
    exit(1)

client = Client()
client.run()