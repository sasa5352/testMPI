comp=gfortran
pattern=*.f90
paral=-fopenmp
opt=-O2
deb=-g#-debug
source=$(wildcard $(pattern))
obj=$(patsubst %.f90, %.o, $(source))

Kadane: $(obj)
	$(comp) $(opt) $^ -o $@ $(paral) $(deb)
main.o:Task.mod
%.o %.mod : %.f90
	$(comp) -c $(opt) $< $(paral) $(deb)
clear:
	rm -f *.o *.mod prog *.eps result.dat
ex: Kadane
	./Kadane
