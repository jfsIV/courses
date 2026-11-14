problem_name = "test_a"

region_boundaries = [0, 1, 11]
sigma_ts = [2, 100]
sigma_ss = [0, 100]
sigma_fs = [0,   0]
qs = [0, 0]

left_bc = "incident"
left_incident_flux = 1
right_bc = "vacuum"

delta_xs = [0.1, 1]

double_legendre_order = 4

convergence_criteria = 1e-10
