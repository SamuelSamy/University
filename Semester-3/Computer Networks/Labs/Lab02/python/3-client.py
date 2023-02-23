import socket
import struct

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
        number = float(input('Enter float number: '))
        self.socket.send(struct.pack('!f', number))

        length = self.socket.recv(4)
        length = struct.unpack('!i', length)[0]

        data = self.socket.recv(length)
        data = struct.unpack(f'!{length}s', data)[0]
        data = data.decode('utf-8')

        print(data)


client = Client()
client.run()