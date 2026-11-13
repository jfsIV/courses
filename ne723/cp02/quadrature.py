import numpy as np


def discretize_angle(problem):
    order = problem.double_legendre_order

    xi, w_base = np.polynomial.legendre.leggauss(order)

    # negative half-range
    mu_neg = 0.5 * (xi - 1.0)
    w_neg  = 0.5 * w_base

    # positive half-range
    mu_pos = 0.5 * (xi + 1.0)
    w_pos  = 0.5 * w_base

    # combined
    mu = np.concatenate((mu_neg, mu_pos))
    w  = np.concatenate((w_neg, w_pos))

    quad_dict = {
        "n_angles": len(mu),
        "mu": mu,
        "w": w,
    }

    return quad_dict
