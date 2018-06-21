FC=gfortran
FFLAGS=-O3 -fcheck=bounds
LFLAGS=-rcs
LC=ar

.PHONY: all

objects = dynamic_arrays.o dynamic_arrays.mod hello.o hello.mod hash_tables.o hash_tables.mod myUtils.o myUtils.mod 

all: $(objects) 

dynamic_arrays.o : dynamic_arrays.f90
	$(FC) $(FFLAGS) -c dynamic_arrays.f90 
dynamic_arrays.mod : dynamic_arrays.f90 dynamic_arrays.o
	$(FC) $(FFLAGS) -c dynamic_arrays.f90 
hello.o : hello.f90
	$(FC) $(FFLAGS) -c hello.f90
hello.mod : hello.f90 hello.o
	$(FC) $(FFLAGS) -c hello.f90
hash_tables.o : hash_tables.f90
	$(FC) $(FFLAGS) -c hash_tables.f90
hash_tables.mod : hash_tables.f90 hash_tables.o
	$(FC) $(FFLAGS) -c hash_tables.f90
myUtils.o : myUtils.f90
	$(FC) $(FFLAGS) -c myUtils.f90
myUtils.mod : myUtils.f90 myUtils.o
	$(FC) $(FFLAGS) -c myUtils.f90

%.mod: %.f90 %.o
	$(FC) $(FFLAGS) -c $^ -o $@

%.o : %.f90
	$(FC) $(FFLAGS) -c $^ -o $<


clean: 
	rm -f *.o *.mod


