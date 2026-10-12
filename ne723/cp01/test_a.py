# input parameters for "test_a"
problem_type = "fixed_source"
problem_name = "test_a"

convergence_criteria = 1e-6

uniform_dx = True
n_mesh_cells = 40
legendre_order = 8

left_bc  = "reflective"
right_bc = "vacuum"

region_boundaries = [0, 2, 3, 5, 6, 8]
sigma_ts = [50, 5, 0, 1.1, 1.1]
sigma_ss = [ 0, 0, 0, 0.7, 0.5]
sigma_fs = [ 0, 0, 0, 0.1, 0.16]

nu_fs = [ 0, 0, 0, 2, 2.5]
qs    = [50, 0, 0, 0, 1]
