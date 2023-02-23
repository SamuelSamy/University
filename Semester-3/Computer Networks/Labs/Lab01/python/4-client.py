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
        s0 = input('Enter string0: ')
        s1 = input('Enter string1: ')

        self.socket.send(s0.encode())
        self.socket.send(s1.encode())

        result = self.socket.recv(4096).decode()
        print(f'Got {result} from server.')


client = Client()
client.run()