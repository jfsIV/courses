FC := gfortran
FCFLAGS := -c -O2
BUILD_DIR := build
SRC_DIR := src

FILES := main.f90 \
		globals.f90 \
		versioning.f90 \
		input.f90 \
		solver.f90

OBJS := $(FILES:%.f90=$(BUILD_DIR)/%.o)
PROGRAM := sn_2d


all: $(PROGRAM)

# link
$(PROGRAM): $(OBJS)
	@$(FC) -o $@ $^

# compile
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	@$(FC) $(FCFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -o $@ $<

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)


$(BUILD_DIR)/main.o: \
	$(BUILD_DIR)/versioning.o \
	$(BUILD_DIR)/globals.o \
	$(BUILD_DIR)/input.o \
	$(BUILD_DIR)/solver.o

$(BUILD_DIR)/versioning.o: \
	$(BUILD_DIR)/globals.o

$(BUILD_DIR)/input.o: \
	$(BUILD_DIR)/globals.o

clean:
	@rm -rf build sn_2d
