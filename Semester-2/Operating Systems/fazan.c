#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <time.h>

char startingLetters[] = "aa, ab, ac, ad, ae, af, ag, ah, ai, aj, ak, al, am, an, ao, ap, ar, as, at, au, av, ax, ay, az, ba, be, bi, bl, bo, br, bu, by, ca, ce, ch, ci, cl, cn, co, cr, ct, cu, cv, da, de, di, do, dr, du, dv, ea, eb, ec, ed, ef, eg, eh, ei, ej, el, em, en, eo, ep, er, es, et, eu, ev, ex, ez, fa, fe, fi, fl, fo, fr, ft, fu, ga, ge, gh, gi, gl, gn, go, gr, gu, ha, he, hi, hl, hm, ho, hr, ht, hu, ia, ib, ic, id, ie, if, ig, ih, ii, ik, il, im, in, io, ip, ir, is, it, iu, iv, iz, ja, jd, je, jg, ji, jn, jo, ju, ka, ke, kh, ki, ko, kr, ku, la, le, li, lo, lu, ma, md, me, mi, ml, mn, mo, mr, mu, na, ne, ni, no, nu, oa, ob, oc, od, of, og, oh, oi, oj, ol, om, on, oo, op, or, os, ot, ou, ov, ox, oz, pa, pe, pf, ph, pi, pl, pn, po, pr, ps, pt, pu, qu, ra, re, ri, ro, ru, sa, sc, se, sf, sg, sh, si, sk, sl, sm, sn, so, sp, ss, st, su, sv, ta, te, th, ti, tm, to, tr, tu, ub, uc, ud, uf, ug, uh, ui, uj, ul, um, un, up, ur, us, ut, uv, uz, va, ve, vi, vl, vo, vr, vu, wa, we, wi, wo, wu, xa, xe, xi, ya, ye, yo, yt, yu, za, zb, zd, ze, zg, zi, zl, zm, zn, zo, zu, zv";

const int MAX_DEATHS = 2;
const int VALID_PREFIX_STATUS = 3;

void makeFifos(int numberOfPlayers, int startingPlayer, int playerNext[], int playerPrev[])
{
    int currentPlayer = startingPlayer;
    do
    {
        int nextPlayer = playerNext[currentPlayer];

        char file[256];
        sprintf(file, "fifo%d-%d", currentPlayer, nextPlayer);
        mkfifo(file, 0667);

        currentPlayer = nextPlayer;
    } while (currentPlayer != startingPlayer);
    
}

void unlinkFifos(int numberOfPlayers, int startingPlayer, int playerNext[], int playerPrev[])
{
    int currentPlayer = startingPlayer;
    do
    {
        int nextPlayer = playerNext[currentPlayer];

        char file[256];
        sprintf(file, "fifo%d-%d", currentPlayer, nextPlayer);
        unlink(file);

        currentPlayer = nextPlayer;
    } while (currentPlayer != startingPlayer);
}

int isPrefix(char* word, char* prefix)
{
    return strncmp(prefix, word, strlen(prefix)) == 0;
}

int canCreateWord(char* prefix)
{
    return !(strstr(startingLetters, prefix) == 0);
}

int validateWord(char* word, char* prefix)
{
    if (strlen(word) < 3)
    {
        printf("The word must have at least 3 letters!\n");
        return 0;
    }

    if (!isPrefix(word, prefix))
    {
        printf("The word does not start with the specified prefix!\n");
        return 0;
    }

    char starting[3];
    starting[0] = word[0];
    starting[1] = word[1];
    starting[2] = '\0';

    if (!canCreateWord(starting))
    {
        printf("This word does not exist!\n");
        return 0;
    }

    return 1;
}

void startGame(int player, int playerNext[], int playerPrev[]);

void initializeGame(int player, char letter, int playerNext[], int playerPrev[])
{
    char toPrint[128];
    sprintf(toPrint, "Enter word starting with the letter `%c` - Player %d: ", letter, player);
    
    char word[64];
    char prefix[3];
    prefix[0] = letter;
    prefix[1] = '\0';

    char ending[3];
    
    while (1)
    {
        printf("%s", toPrint);
        memset(word, 0, 64);
        scanf("%s", word);

        if (!validateWord(word, prefix))
        {
            continue;
        }

        ending[0] = word[strlen(word) - 2];
        ending[1] = word[strlen(word) - 1];
        ending[2] = '\0';

        if (!canCreateWord(ending))
        {
            printf("You can not end the game in the first round!\n");
            continue;
        }

        break;
    } 
    
    char file[256];
    sprintf(file, "fifo%d-%d", player, playerNext[player]);

    int three = VALID_PREFIX_STATUS;
    int fd = open(file, O_WRONLY);
    write(fd, &three, sizeof(int));
    write(fd, ending, sizeof(char) * 3);
    close(fd);

    startGame(player, playerNext, playerPrev);
}

void startGame(int player, int playerNext[], int playerPrev[])
{
    while (1)
    {
        char fromFifo[64];
        sprintf(fromFifo, "fifo%d-%d", playerPrev[player], player);

        char toFifo[64];
        sprintf(toFifo, "fifo%d-%d", player, playerNext[player]);

        int status;
        char prefix[3];

        
        int fd = open(fromFifo, O_RDONLY);
        read(fd, &status, sizeof(int));
        read(fd, prefix, sizeof(char) * 2);
        close(fd);
        
        prefix[2] = '\0';

        if (status != VALID_PREFIX_STATUS)
        {
            int deadPlayer = status - 1000;

            if (playerNext[player] != deadPlayer)
            {
                fd = open(toFifo, O_WRONLY);
                write(fd, &status, sizeof(int));
                close(fd);
            }

            exit(0);
        }

        if (!canCreateWord(prefix))
        {
            // the round ends, this player died
            int status = 1000 + player;
            int fd = open(toFifo, O_WRONLY);
            write(fd, &status, sizeof(int));
            close(fd);

            // write to parent
            fd = open("fifoDead", O_WRONLY);
            write(fd, &player, sizeof(int));
            close(fd);

            printf("\nPlayer %d lost this round!\n\n", player);

            exit(0);
        }

        // read the word
        char word[64];
        char toPrint[128];
        sprintf(toPrint, "Enter word starting with `%s` - Player %d: ", prefix, player);

        while (1)
        {
            printf("%s", toPrint);
            memset(word, 0, 64);
            scanf("%s", word);

            if (!strcmp(word, "-"))
            {
                // the round ends, this player died
                int status = 1000 + player;
                int fd = open(toFifo, O_WRONLY);
                write(fd, &status, sizeof(int));
                close(fd);

                // write to parent
                fd = open("fifoDead", O_WRONLY);
                write(fd, &player, sizeof(int));
                close(fd);

                printf("\nPlayer %d lost this round!\n\n", player);

                exit(0);
            }

            if (!validateWord(word, prefix))
            {
                continue;
            }

            break;
        } 

        // write the sufix in the next fifo
        char ending[3];
        ending[0] = word[strlen(word) - 2];
        ending[1] = word[strlen(word) - 1];
        ending[2] = '\0';

        fd = open(toFifo, O_WRONLY);
        write(fd, &status, sizeof(int));
        write(fd, &ending, sizeof(char) * 3);
        close(fd);
    }
}


int getActivePlayers(int startingPlayer, int playerNext[])
{
    int currentPlayer = startingPlayer;
    int players = 0;
    
    do
    {
        players++;
        currentPlayer = playerNext[currentPlayer];
    } while (currentPlayer != startingPlayer);

    return players;
}

void printFazanStatus(int numberOfPlayers, int deaths[])
{
    printf("\n\n\"Fazan\" state:\n");
    
    int i;
    for (i = 0; i < numberOfPlayers; i++)
    {
        char toPrint[64];
        char fazan[10] = "fazan";

        sprintf(toPrint, "Player %d: ", i);
    
        int j;
        int last = deaths[i];
        if (last == -1)
        {
            last = 5;
        }

        for (j = 0; j < last; j++)
        {
            sprintf(toPrint, "%s%c", toPrint, fazan[j]);
        }

        printf("%s\n", toPrint);
    }

    printf("\n\n");
}


int main()
{
    srand(time(0));

    int numberOfPlayers;
    printf("Enter the number of players: ");
    scanf("%d", &numberOfPlayers);

    if (numberOfPlayers < 2)
    {
        printf("Invalid number of players (must be at least 2)\n");
        exit(1);
    }

    int i;
    int playerNext[64];
    int playerPrev[64];

    for (i = 0; i < numberOfPlayers; i++)
    {
        playerNext[i] = i + 1;
        playerPrev[i] = i - 1;
    }

    playerNext[numberOfPlayers - 1] = 0;
    playerPrev[0] = numberOfPlayers - 1;

    mkfifo("fifoStatus", 0666);
    mkfifo("fifoDead", 0666);

    printf("\n\nIf you do not know a word type the minus sign (-)\nStarting game...\n\n");

    int mainChildID = fork();
    if (mainChildID < 0)
    {
        printf("error");
        exit(1);
    }

    if (mainChildID == 0)
    {
        while (1)
        {
            // read data
            int i;
            int fd = open("fifoStatus", O_RDONLY);
            int numberOfPlayers;
            int startingPlayer;
            char startingLetter;
            int deaths[64];

            read(fd, &numberOfPlayers, sizeof(int));

            if (numberOfPlayers < 0)
            {
                exit(0);
            }

            read(fd, &startingPlayer, sizeof(int));
            read(fd, &startingLetter, sizeof(char));

            for (i = 0; i < numberOfPlayers; i++)
            {
                read(fd, &deaths[i], sizeof(int));
            }
            close(fd);

            // create fifos
            makeFifos(numberOfPlayers, startingPlayer, playerNext, playerPrev);

            int currentPlayer = playerNext[startingPlayer];
            
            int startingID = fork();
            if (startingID == 0)
            {
                initializeGame(startingPlayer, startingLetter, playerNext, playerPrev);
                exit(0);
            }

            while (currentPlayer != startingPlayer)
            {
                int pid = fork();
                if (pid == 0)
                {
                    startGame(currentPlayer, playerNext, playerPrev);
                    exit(0);
                }

                currentPlayer = playerNext[currentPlayer];
            }

            // read the dead player
            fd = open("fifoDead", O_RDONLY);
            int deadPlayer = -1;
            read(fd, &deadPlayer, sizeof(int));
            close(fd);
            deaths[deadPlayer]++;
        

            // wait for all the children
            int activePlayers = getActivePlayers(startingPlayer, playerNext);
            
            for (i = 0; i < activePlayers; i++)
            {
                wait(0);
            }

            // destroy fifos
            unlinkFifos(numberOfPlayers, startingPlayer, playerNext, playerPrev);

            // write data to status
            fd = open("fifoStatus", O_WRONLY);
            write(fd, &numberOfPlayers, sizeof(int));
            write(fd, &startingPlayer, sizeof(int));
            write(fd, &startingLetter, sizeof(char));

            for (i = 0; i < numberOfPlayers; i++)
            {
                if (deaths[i] == MAX_DEATHS)
                {
                    playerNext[playerPrev[i]] = playerNext[i];
                    playerPrev[playerNext[i]] = playerPrev[i];
                }
                write(fd, &deaths[i], sizeof(int));
            }
            close(fd);
        }
    }

    int startingPlayer = 0;
    int deaths[64] = { 0 };
    int activePlayers = getActivePlayers(startingPlayer, playerNext);

    while (activePlayers > 1)
    {
        char letter = (char)(rand() % 26 + 97);
        int fd = open("fifoStatus", O_WRONLY);
        write(fd, &numberOfPlayers, sizeof(int));
        write(fd, &startingPlayer, sizeof(int));
        write(fd, &letter, sizeof(char));

        for (i = 0; i < numberOfPlayers; i++)
        {
            write(fd, &deaths[i], sizeof(int));
        }

        close(fd);

        printFazanStatus(numberOfPlayers, deaths);

        fd = open("fifoStatus", O_RDONLY);
        read(fd, &numberOfPlayers, sizeof(int));
        read(fd, &startingPlayer, sizeof(int));
        read(fd, &letter, sizeof(char));

        for (i = 0; i < numberOfPlayers; i++)
        {
            read(fd, &deaths[i], sizeof(int));

            // a player died last round
            if (deaths[i] == MAX_DEATHS)
            {
                playerNext[playerPrev[i]] = playerNext[i];
                playerPrev[playerNext[i]] = playerPrev[i];
                printf("\n\nPlayer %i eliminated\n\n", i);
                deaths[i] = -1;
            }
        }

        close(fd);

        startingPlayer = playerNext[startingPlayer];
        activePlayers = getActivePlayers(startingPlayer, playerNext);
    }
   
    printFazanStatus(numberOfPlayers, deaths);

    numberOfPlayers = -1;
    int fd = open("fifoStatus", O_WRONLY);
    write(fd, &numberOfPlayers, sizeof(int));
    close(fd);
    
    wait(0);
    
    printf("Player %d wins the game!\n", startingPlayer);

    unlink("fifoStatus");
    unlink("fifoDead");

    return 0;
}
