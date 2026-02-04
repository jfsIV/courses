FC := gfortran
FCFLAGS := -c -O2
BUILD_DIR := build

SRCS := main.f90 version_data.f90 globals.f90 input_parser.f90
OBJS := $(SRCS:%.f90=$(BUILD_DIR)/%.o)
PROGRAM := sn_2d


all: $(PROGRAM)

# link
$(PROGRAM): $(OBJS)
	@$(FC) -o $@ $^

# compile
$(BUILD_DIR)/%.o: %.f90 | $(BUILD_DIR)
	@$(FC) $(FCFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -o $@ $<

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/main.o: $(BUILD_DIR)/version_data.o $(BUILD_DIR)/globals.o $(BUILD_DIR)/input_parser.o
$(BUILD_DIR)/version_data.o: $(BUILD_DIR)/globals.o
$(BUILD_DIR)/input_parser.o: $(BUILD_DIR)/globals.o

clean:
	@rm -rf build sn_2d

#OBJS := $(SRCS:%.f90=$(BUILD_DIR)/%.o)
#BUILD_DIR := ./build

#all: $(PROGRAM)

#$(PROGRAM): $(OBJS)
#	$(FC) -o $@ $^

#$(BUILD_DIR)%.o: %.f90
#	$(FC) -o $@ $<

#$(BUILD_DIR)/$(PROGRAM): $(OBJS)
#	mkdir -p $(dir $@)
#	$(FC) $(OBJS) -o $@
