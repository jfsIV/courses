import numpy as np


def discretize_space(problem):
    cell_edges  = np.array(region_boundaries)
    cell_widths = cell_edges[1:] - cell_edges[:-1]

    cell_widths = problem.region_boundaries[1:]

    return
