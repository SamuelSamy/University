import socket
import struct
import threading
import typing

PORT = 55555

class Server:

    def __init__(self):
        self.sorted_array: typing.List[float] = []
        self.sockets: typing.List[socket.socket] = []
        self.threads: typing.List[threading.Thread] = []
        
        self.lock: threading.Lock = threading.Lock()

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
            try:
                connection_socket, address = self.socket.accept()
            except:
                print('Server closed.')
                break
            
            print(f'Connected {address}')

            self.sockets.append(connection_socket)
            t = threading.Thread(target = self.worker, args = (connection_socket, address))
            self.threads.append(t)
            t.start()
            


    def worker(self, _socket: socket.socket, address):
        
        while True:
            
            try:
                array = self.recv_array(_socket)
            except:
                print('Client disconnected.')
                self.sockets.remove(_socket)
                break

            print(f'Received array: {array} from {address}')


            self.lock.acquire()
            
            self.sorted_array += array
            self.sorted_array.sort()

            for socket in self.sockets:
                self.send_number(socket, len(array), '!i')
                self.send_array(socket, self.sorted_array)

            self.lock.release()

            if len(array) == 0:
                print('The array is empty...closing sockets')
                self.lock.acquire()

                for socket in self.sockets:
                    if socket != _socket:
                        self.send_number(socket, 0, '!i')
                        socket.close()

                self.socket.close()
                break
                
        _socket.close()
        exit(0)


    def send_number(self, socket, number, number_format: typing.Literal['!i', '!f']):
        socket.send(struct.pack(number_format, number))


    def send_string(self, socket, string):
        socket.send(struct.pack(string.encode()))


    def recv_number(self, socket, number_format: typing.Literal['!i', '!f']):
        data = socket.recv(4)
        data = struct.unpack(number_format, data)[0]
        return data

    
    def recv_string(self, socket):
        data = socket.recv(4096)
        data = data.decode('utf-8')
        return data


    def send_array(self, socket, array):
        self.send_number(socket, len(array), '!i')
        for element in array:
            self.send_number(socket, element, '!f')


    def recv_array(self, socket):
        array = []
        length = self.recv_number(socket, '!i')
        for _ in range(length):
            element = self.recv_number(socket, '!f')
            array.append(element)
        return array


server = Server()
server.run()
