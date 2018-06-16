FC=gfortran
FFLAGS=-O3 -fcheck=bounds
#LFLAGS=-rcs
LFLAGS=cr
LC=ar

SRC_DIR := ./src
OBJ_DIR := ./src
LIB_DIR := ./lib
SRC_FILES := $(wildcard $(SRC_DIR)/*.f90)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.f90,$(OBJ_DIR)/%.o,$(SRC_FILES))

all: $(OBJ_FILES)
	$(LC) $(LFLAGS) $(LIB_DIR)/myUtils.a $(OBJ_FILES)
	$(FC) $(FFLAGS) -o ./test/test.o ./test/test.f90 $(LIB_DIR)/myUtils.a 


$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFALGS) -c $^ -o $@
