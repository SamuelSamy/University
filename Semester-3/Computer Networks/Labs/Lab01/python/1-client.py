import socket
import struct

HOST = '127.0.0.1'
PORT = 55555


with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as _socket:
    try:
        _socket.connect((HOST, PORT))
    except:
        print("Connection error")
        exit(1)

    array = input("Enter array: ")
    array = array.split()

    _socket.send(struct.pack('!I', len(array)))

    for i in array:
        _socket.send(struct.pack('!I', int(i)))


    sum = _socket.recv(4)
    sum = struct.unpack('!I', sum)[0]
    print("Sum =", sum)

