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
        string = input('Enter string: ')
        self.socket.send(string.encode())

        data = self.socket.recv(4096)
        data = struct.unpack('!q', data)[0]
        print(f'Got {data} from server.')


client = Client()
client.run()