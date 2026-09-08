# Compilation
To compile the code, run:

```
1. make
```

To recompile the code, run:

```
1. make clean
2. make
```


# Execution
Make sure you are in the project home directory. To execute the code, run:

```
./sn_2d <input_file=sample.i>
```

By default, `sn_2d` will execute using `sample.i`, but you can pass a different file name as a command line argument.


# Status
The code is operational


# Description
This code reads input files, makes sure the input file is properly formatted, and writes an output file for those variables.

## Naming Input Files
The expected input is a file named `*.i`. Every entry in the input file needs to be seperated by at least one space. However, The input file is read using free-format parsing, so line breaks do not affect input reading.

## Input Syntax
The expected input syntax is as follows:
1. I J: the number of cells in the X,Y directions (2 integers)
2. cell size in the x-direction (I reals)
3. cell size in the y-direction (J reals)
4. K: the number of angles per octant (1 integer)
5. mu_i, eta_i, w_i: angular quadrature repeated as many times as angles per octant (3 reals, K times)
6. M: the number of materials (1 integer)
7. Sigma_T^i, Sigma_S^i: the total and scattering cross sections for each material (2 reals, M times)
8. Left-, Right-, Bottom-, Top Boundary Condition: 0 for vacuum, 1 for reflective
9. An IxJ array where each entry is the material in each respective cell
10. An IxJ array where each entry is the source in each respective cell

## Limitations and Restrictions
The code only reads input files and writes an output file, so no transport calculations are being performed. (yet...)
