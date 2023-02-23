import socket
import struct
import threading
import datetime
import random

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

        self.lock = threading.Lock()
        self.clients = {}


    def run(self):
        print('Server is running...')
        self.number = random.uniform(0, 1000)
        print('Number is: ', self.number)
        
        self.finsihed = False
        
        while not self.finsihed:

            try:
                self.socket.settimeout(10)

                print('Waiting for connections...')
                connection_socket, address = self.socket.accept()
                print(f'Client connected on {address}')

                t = threading.Thread(
                    target = self.worker,
                    args = (connection_socket,)
                )
                
                self.lock.acquire()
                self.clients[connection_socket] = [t, None]
                self.lock.release()

                t.start()


            except socket.timeout:
                print('Timeout')
                self.end_game()
                self.finsihed = True

            finally:
                self.socket.settimeout(None)
            
        self.socket.close()


    def end_game(self):
        closest_diff = None
        winner = None

        for client, data in self.clients.items():
            if closest_diff == None:
                closest_diff = abs(self.number - data[1])
                winner = client
                continue

            if abs(self.number - data[1]) < closest_diff:
                closest_diff = abs(self.number - data[1])
                winner = client

        print(winner, closest_diff)

        for client in self.clients.keys():
            client: socket.socket
            text = 'You lost!'

            if client == winner:
                text = 'You have the best guess with an error of ' + str(closest_diff)

            client.sendall(struct.pack('!i', len(text)))
            client.sendall(bytes(text, 'ascii'))
            client.close()


    def worker(self, _socket: socket.socket):
        guess = struct.unpack('!f', _socket.recv(8))[0]
        self.clients[_socket][1] = guess
        exit(0)


server = Server()
server.run()