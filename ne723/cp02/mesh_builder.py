import numpy as np


def discretize_space(problem):
    region_edges   = np.array(problem.region_boundaries)
    region_widths  = region_edges[1:] - region_edges[:-1]
    delta_xs = np.array(problem.delta_xs)

    n_cells_per_region = np.round(region_widths / delta_xs).astype(int)
    n_cells_total = np.sum(n_cells_per_region)
    n_regions = len(region_widths)

    cell_edges = [region_edges[0]]
    for region_idx in range(n_regions):
        sub_edges = np.linspace(
            region_edges[region_idx],
            region_edges[region_idx + 1],
            n_cells_per_region[region_idx] + 1,
        )
        cell_edges.extend(sub_edges[1:])

    cell_edges   = np.array(cell_edges)
    cell_widths  = cell_edges[1:] - cell_edges[:-1]
    cell_centers = 0.5 * (cell_edges[1:] + cell_edges[:-1])

    sigma_t = np.zeros(n_cells_total)
    sigma_s = np.zeros(n_cells_total)
    sigma_f = np.zeros(n_cells_total)
    q       = np.zeros(n_cells_total)

    start_idx = 0
    for reg_idx in range(n_regions):
        end_idx = start_idx + n_cells_per_region[reg_idx]
        sigma_t[start_idx:end_idx] = problem.sigma_ts[reg_idx]
        sigma_s[start_idx:end_idx] = problem.sigma_ss[reg_idx]
        sigma_f[start_idx:end_idx] = problem.sigma_fs[reg_idx]
        q[start_idx:end_idx]       = problem.qs[reg_idx]
        start_idx = end_idx

    mesh_dict = {
        "n_cells": n_cells_total,
        "cell_edges": cell_edges,
        "cell_widths": cell_widths,
        "cell_centers": cell_centers,
        "sigma_t": sigma_t,
        "sigma_s": sigma_s,
        "sigma_f": sigma_f,
        "q": q,
    }

    return mesh_dict
