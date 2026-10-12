# input parameters for "test_c"
problem_type = "k-eigenvalue"
problem_name = "test_c"

convergence_criteria = 1e-6

uniform_dx = True
n_mesh_cells = 50
legendre_order = 8

left_bc  = "vacuum"
right_bc = "vacuum"

# the second region boundary is the initial guess for a
region_boundaries = [0, 1.0]
sigma_ts = [1]
sigma_ss = [0.45]
sigma_fs = [0.3]

nu_fs = [2]
qs    = [0]
