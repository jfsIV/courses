# input parameters for "test_b"
problem_type = "fixed_source"
problem_name = "test_b"

convergence_criteria = 1e-6

uniform_dx = False
n_mesh_cells = [10, 10]
legendre_order = 8

left_bc = "incident"
left_incident_flux = 1
right_bc = "vacuum"

region_boundaries = [0, 1, 11]
sigma_ts = [2, 100]
sigma_ss = [0, 100]
sigma_fs = [0,   0]

nu_fs = [0, 0]
qs    = [0, 0]
