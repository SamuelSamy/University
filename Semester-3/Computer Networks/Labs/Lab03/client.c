#include <stdio.h>
#include <WinSock2.h>
#include <stdint.h>
#include <stdio.h>

#define IP_ADDRESS "172.30.114.86"
#define PORT 55555

int main(int argc, char** argv)
{
    WSADATA wsaData;
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) < 0)
    {
        printf("Error initializing the Windows Sockets Library\n");
        return -1;
    }

    SOCKET communicationSocket;
    struct sockaddr_in server;

    communicationSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (communicationSocket < 0)
    {
        printf("Errror creating socket...\n");
        return 1;
    }

    memset(&server, 0, sizeof(server));
    server.sin_port = htons(PORT);
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = inet_addr(IP_ADDRESS);

    if (connect(communicationSocket, (struct sockaddr*)&server, sizeof(server)) < 0)
    {
        printf("Error connecting to the server\n");
        return 2;
    }

    printf("Enter command: ");
    char* cmd = (char*)malloc(sizeof(char) * 1024);
    fgets(cmd, 1024, stdin);
    uint32_t len = strlen(cmd); 
    len = htonl(len);

    send(communicationSocket, ( char*)&len, sizeof(len), 0);
    send(communicationSocket, cmd, sizeof(cmd), 0);

    uint32_t outputLen;
    char* cmdResult;

    recv(communicationSocket, (char*)&outputLen, sizeof(outputLen), 0);
    outputLen = ntohl(outputLen);
    printf("Outputlen: %d\n", outputLen);
    cmdResult = (char*)malloc(sizeof(char) * outputLen);
    recv(communicationSocket, cmdResult, outputLen, 0);
    printf("The result is:\n%s", cmdResult);

    return 0;
}