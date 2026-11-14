problem_name = "test_d2"

# calcs
nu = 1e-4

sigma_t = 1 / nu
sigma_a = nu
sigma_s = sigma_t - sigma_a


# variables to import
region_boundaries = [0, 1]
sigma_ts = [sigma_t]
sigma_ss = [sigma_s]
sigma_fs = [0]
qs = [nu]

left_bc = "vacuum"
right_bc = "vacuum"

delta_xs = [0.1]

double_legendre_order = 4

convergence_criteria = 1e-10
