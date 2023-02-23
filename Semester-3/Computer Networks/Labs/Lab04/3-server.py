import socket
import typing
import select

HOST = '0.0.0.0'
PORT = '55555'

clients: typing.List[socket.socket] = []

def main():
    try:
        _socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        _socket.bind((HOST, PORT))
        _socket.listen(10)
    except socket.error as message:
        print(message.strerror)
        exit(-1)

    clients.append(_socket)

    while True:
        read, _, _ = select.select(clients, [], [])

        for rs in read:
            rs: socket.socket = rs

            if rs is _socket:
                client, address = rs.accept()
                clients.append(client)
                send_all()

            else:
                data = rs.recv(1024)

                if not data or 'quit' in data.lower():
                    rs.close()
                    clients.remove(rs)
                    send_all()


def send_all():
    data = []
    for client in clients[1:]:
        ip, port = client.getpeername()     
        data.append((ip, port))
    # encode the data and send it, decode on server
    # or pickle??
        



if __name__ == "__main__":
    main()