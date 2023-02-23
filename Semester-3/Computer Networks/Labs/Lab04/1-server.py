import socket
s=socket.socket(socket.AF_INET,socket.SOCK_DGRAM)
s.bind(('0.0.0.0',1234))
while(1):
    d,address=s.recvfrom(120)
    print(d.decode())
    print(address)
    s.sendto(d,address)