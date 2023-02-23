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
        number = input('Enter number: ')
        try:
            number = int(number)
        except ValueError:
            print('Invalid number')
            return

        self.socket.send(struct.pack('!q', number))

        data = self.socket.recv(4096)
        data = data.decode()
        print(f'Got {data} from server.')


client = Client()
client.run()