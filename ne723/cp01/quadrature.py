import numpy as np


def discretize_angle(problem):
    order = problem.legendre_order

    mu, w = np.polynomial.legendre.leggauss(order)

    quad_dict = {
        "mu": mu,
        "w": w,
    }

    return quad_dict
