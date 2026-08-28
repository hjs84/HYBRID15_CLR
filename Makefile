FC = mpif90 -I/usr/local/software/spack/spack-views/.rhel8-icelake-202110272/uxqqj4xcjrltatqgtuoi2hp46uabtzom/intel-oneapi-mpi-2021.4.0/intel-2021.4.0/kypfgtnfzspxoby7tqy7yt6ykejpwk5n/mpi/2021.4.0/
FFLAGS = -O0 -g -traceback -check all -fpe0
BUILD_DIR = build
SRC_DIR = src
EXE = $(BUILD_DIR)/HYBRID15_CLR.exe

SOURCES = \
	$(SRC_DIR)/PARS_MOD.F90				\
	$(SRC_DIR)/VARS_MOD.F90				\
	$(SRC_DIR)/INIT.F90				\
	$(SRC_DIR)/READ_HYBRID15_CLR_FORCING.F90	\
	$(SRC_DIR)/CROWN.F90				\
	$(SRC_DIR)/LEAF.F90				\
	$(SRC_DIR)/HYDRO.F90				\
	$(SRC_DIR)/GROW.F90				\
	$(SRC_DIR)/DECOMP.F90				\
        $(SRC_DIR)/SOILTEMP.F90                         \
	$(SRC_DIR)/HYBRID15_CLR.F90

OBJECTS = $(patsubst $(SRC_DIR)/%.F90,$(BUILD_DIR)/%.o,$(SOURCES))

.PHONY: all clean run

all: $(EXE)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

#$(BUILD_DIR)/%.o: $(SRC_DIR)/%.F90 | $(BUILD_DIR)
#	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.F90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -module $(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

$(EXE): $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) -o $(EXE)

run: $(EXE)
	./$(EXE)

clean:
	rm -rf $(BUILD_DIR)/*.o $(BUILD_DIR)/*.mod $(EXE)



