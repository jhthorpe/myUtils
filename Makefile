FC=gfortran
FFLAGS=-O3 -fcheck=bounds
LFLAGS=-rcs
LC=ar

SRC_DIR := ./src
LIB_DIR := ./lib
SRC_FILES := $(wildcard $(SRC_DIR)/*.f90)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.f90,$(SRC_DIR)/%.o,$(SRC_FILES))
MOD_FILES := $(patsubst $(SRC_DIR)/%.f90,$(SRC_DIR)/%.mod,$(SRC_FILES))

.PHONY: all

#all: $(OBJ_FILES) $(MOD_FILES)
#	echo $(MOD_FILES)
	#if [ -f *.mod ]; then mv *.mod $(LIB_DIR) ; fi	
#	$(LC) $(LFLAGS) $(LIB_DIR)/myUtils.a $(OBJ_FILES) $(MOD_FILES)

#$(SRC_DIR)/%.o : $(SRC_DIR)/%.f90
#	$(FC) $(FFALGS) -c $^ -o $@

#$(SRC_DIR)/%.mod: $(SRC_DIR)/%.f90 $(SRC_DIR)/%.o
#	$(FC) $(FFLAGS) -c $^ -o $@

#$(SRC_DIR)/%.o: $(SRC_DIR)/%.f90

all: 
	@set -e; \
	for i in $(SRC_DIR); do \
		if [ -d $$i ]; then \
			if [ -f $$i/Makefile ]; then \
				$(MAKE) -C $$i all ;\
			fi; \
		fi; \
	done;
	if [ ! -d $(LIB_DIR) ]; then mkdir -p $(LIB_DIR); fi 
	cp $(SRC_DIR)/*.mod $(LIB_DIR)
	$(LC) $(LFLAGS) $(LIB_DIR)/myUtils.a $(OBJ_FILES)
	$(FC) $(FFLAGS) -o ./test/test.o ./test/test.f90 $(LIB_DIR)/myUtils.a -I$(LIB_DIR) 

clean: 
	rm -f $(SRC_DIR)/*.o $(SRC_DIR)/*.mod


