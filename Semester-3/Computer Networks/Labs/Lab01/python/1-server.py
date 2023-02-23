# A client sends to the server an array of numbers. Server returns the sum of the numbers.

import socket
import struct
import threading

PORT = 55555

def worker(_socket: socket.socket):
    nr = _socket.recv(4)
    nr = struct.unpack('!I', nr)[0]
    sum = 0

    for i in range(nr):
        x = _socket.recv(4)
        x = struct.unpack('!I', x)[0]
        sum += x

    _socket.send(struct.pack('!I', sum))
    _socket.close()
    exit(0)


with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as _socket:
    try:
        _socket.bind(('0.0.0.0', PORT))
        _socket.listen(5)
    except:
        print("Bind error")
        exit(1)

    while True:
        print("Waiting for connection")
        connection_scoket, addr = _socket.accept()
        print(f"Connected {addr}")
        t = threading.Thread(target=worker, args=(connection_scoket,))
        t.start()
