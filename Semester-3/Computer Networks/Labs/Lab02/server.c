#include <stdio.h>
#include <errno.h>
#include <WinSock2.h>
#include <stdint.h>


int main()
{
    SOCKET s;
    struct sockaddr_in server, client;
    int communicationSocket, err, l;

    WSADATA wasData;

    if (WSAStartup(MAKEWORD(2, 2), &wasData) < 0)
    {
        printf("Error - Failed to initialize SOCKET libray\n");
        return -1;
    }

    s = socket(AF_INET, SOCK_STREAM, 0);
    if (s < 0)
    {
        printf("Error - Failed to create server socket\n");
        return -2;
    }

    memset(&server, 0, sizeof(server));

    server.sin_port = htons(55555);
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = INADDR_ANY;

    if (bind(s, (struct sockaddr*)&server, sizeof(server)) < 0)
    {
        printf("Bind error\n");
        return -3;
    }

    listen(s, 5);

    l = sizeof(client);
    

    while (1)
    {
        uint16_t  a, b, sum;

        printf("Listening for numbers...\n");
        memset(&client, 0, sizeof(client));
        communicationSocket = accept(s, (struct sockaddr*)&client, &l);
        err = WSAGetLastError();

        if (communicationSocket < 0)
        {
            printf("Accept error: %d\n", err);
            continue;
        }

        printf("Incomming connected client from: %s:%d\n", inet_ntoa(client.sin_addr), ntohs(client.sin_port));

        int res = recv(communicationSocket, (char*)&a, sizeof(a), 0);
        if (res != sizeof(a))
        {
            printf("Error reciving values...\n");
            continue;
        }

        res = recv(communicationSocket, (char*)&b, sizeof(b), 0);
        if (res != sizeof(b))
        {
            printf("Error reciving values...\n");
            continue;
        }

        a = ntohs(a);
        b = ntohs(b);
        printf("Recived %hu, %hu...\nSending %hu...\n\n", a, b, a + b);
        sum = htons(a + b);
        res = send(communicationSocket, (char*)&sum, sizeof(sum), 0);
        if (res != sizeof(sum))
        {
            printf("Error sending the value...\n");
            continue;
        }

        closesocket(communicationSocket);
    }

    WSACleanup();

    return 0;
}