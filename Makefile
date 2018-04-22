PROGRAM=flp18-log
SRC=ts.pl

CC=swipl
FLAGS=-q

all:
	$(CC) $(FLAGS) -g start -o $(PROGRAM) -c $(SRC)

clean:
	rm $(PROGRAM)