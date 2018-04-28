#Touringov stroj
#Adam Bezak, xbezak01@stud.fit.vutbr.cz
PROGRAM=flp18-log
SRC=xbezak01.pl

CC=swipl
FLAGS=-q

all:
	$(CC) $(FLAGS) -g start -o $(PROGRAM) -c $(SRC)

clean:
	rm $(PROGRAM)