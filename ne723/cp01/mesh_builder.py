import numpy as np


def discretize_space(problem):
    if problem.uniform_dx:
        width = problem.region_boundaries[-1] - problem.region_boundaries[0]
        n_cells = problem.n_mesh_cells
        dx_value = width / n_cells

        dx = np.full(n_cells, dx_value)
        cell_edges = np.linspace(
                problem.region_boundaries[0],
                problem.region_boundaries[-1],
                n_cells + 1)

    if not problem.uniform_dx:
        dx_list = []
        cell_edges_list = [problem.region_boundaries[0]]

        for region_idx in range(len(problem.n_mesh_cells)):
            left_bound = problem.region_boundaries[region_idx]
            right_bound = problem.region_boundaries[region_idx + 1]

            region_width = right_bound - left_bound
            n_region_cells = problem.n_mesh_cells[region_idx]

            region_dx = region_width / n_region_cells
            dx_list.extend([local_dx] * n_region_cells)

            for cell_idx in range(1, n_region_cells + 1):
                cell_edges_list.append(left_bound + cell_idx * local_dx)

        dx = np.array(dx_list)
        cell_edges = np.array(cell_edges_list)

    n_problem_cells = len(dx)
    cell_centers = 0.5 * (cell_edges[:-1] + cell_edges[1:])

    sigma_t = np.zeros(n_problem_cells)
    sigma_s = np.zeros(n_problem_cells)
    sigma_f = np.zeros(n_problem_cells)
    nu_f    = np.zeros(n_problem_cells)
    q       = np.zeros(n_problem_cells)

    for cell_idx in range(n_problem_cells):
        cell_center = cell_centers[cell_idx]

        for region_idx in range(len(problem.sigma_ts)):
            left_bound = problem.region_boundaries[region_idx]
            right_bound = problem.region_boundaries[region_idx + 1]

            if left_bound <= cell_center < right_bound:
                sigma_t[cell_idx] = problem.sigma_ts[region_idx]
                sigma_s[cell_idx] = problem.sigma_ss[region_idx]
                sigma_f[cell_idx] = problem.sigma_fs[region_idx]
                nu_f[cell_idx]    = problem.nu_fs[region_idx]
                q[cell_idx]       = problem.qs[region_idx]
                break

    mesh_dict = {
        "dx": dx,
        "cell_edges": cell_edges,
        "sigma_t": sigma_t,
        "sigma_s": sigma_s,
        "sigma_f": sigma_f,
        "nu_f": nu_f,
        "q": q,
    }

    return mesh_dict
