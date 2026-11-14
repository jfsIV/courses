import numpy as np
from scipy.linalg import solve_banded


def qd_solver(problem, mesh_dict, quad_dict, log_file):
    epsilon = problem.convergence_criteria
    n_cells = mesh_dict["n_cells"]

    mu = quad_dict["mu"]
    w  = quad_dict["w"]

    pos_mask = mu > 0
    neg_mask = mu < 0

    # convergence history
    converged = False

    l_inf_history = []
    rho_history = [0.5, 0.5]

    # initial guesses (for s=0)
    phi_prev = np.ones(n_cells)
    phi_lld = np.ones((n_cells, 2))
    phi_error_prev = None

    E_avg = np.full(n_cells, 1.0 / 3.0)
    E_edge = np.full(n_cells + 1, 1.0 / 3.0)
    C_L = -1/2
    C_R = 1/2

    s = 0
    while not converged:
        s += 1

        if s > 1:
            # high-order LLD mesh sweep
            psi_avg, psi_edge = angular_mesh_sweep(problem, mesh_dict, quad_dict, phi_lld)

            # QD factors
            E_avg  = np.sum((mu[:, None] ** 2) * psi_avg * w[:, None], axis=0) / np.sum(psi_avg * w[:, None], axis=0)
            E_edge = np.sum((mu[:, None] ** 2) * psi_edge * w[:, None], axis=0) / np.sum(psi_edge * w[:, None], axis=0)

            # boundary factors
            C_L = np.sum(mu[neg_mask] * psi_edge[neg_mask, 0] * w[neg_mask]) / np.sum(psi_edge[neg_mask, 0] * w[neg_mask])
            C_R = np.sum(mu[pos_mask] * psi_edge[pos_mask, -1] * w[pos_mask]) / np.sum(psi_edge[pos_mask, -1] * w[pos_mask])

        # solve LOQD problem
        phi_avg, phi_edge, J_edge = fv_loqd_solver(
            problem, mesh_dict, quad_dict,
            E_avg, E_edge, C_L, C_R,
        )

        # apply discrete prolongation operator R_h for cell-edge, LLD-space phi
        phi_lld = np.zeros((n_cells, 2))
        phi_lld[:, 0] = 0.5 * phi_edge[:-1] + phi_avg - 0.5 * phi_edge[1:] # left
        phi_lld[:, 1] = -0.5 * phi_edge[:-1] + phi_avg + 0.5 * phi_edge[1:] # right

        # convergence checks
        l_inf = np.max(np.abs(phi_avg - phi_prev))
        l_inf_history.append(l_inf)

        if s > 2 and phi_error_prev > 0.0:
            rho = l_inf / phi_error_prev
            rho_history.append(rho)
        else:
            rho = rho_history[s - 1]

        msg = f"Iter {s:3d} | L_inf = {l_inf:.6e} | rho = {rho:.6f}\n"
        log_file.write(msg)

        if s >= 2:
            tolerence_criteria = epsilon * (1.0 / rho - 1.0)

            if l_inf <= tolerence_criteria:
                converged = True

        # update for next iteration
        phi_error_prev = l_inf
        phi_prev = phi_avg.copy()

    # sweep with converged values
    psi_avg, psi_edge = angular_mesh_sweep(problem, mesh_dict, quad_dict, phi_lld)
    E_avg, E_edge, C_L, C_R = compute_qd_factors(quad_dict, psi_avg, psi_edge)

    # transport metrics
    M_E = np.abs(1.0 / 3.0 - E_avg)
    M_E_prime = np.abs((E_edge[1:] - E_edge[:-1]) / mesh_dict["cell_widths"])

    results_dict = {
        # discretization variables
        "cell_centers": mesh_dict["cell_centers"],
        "cell_edges": mesh_dict["cell_edges"],
        "cell_widths": mesh_dict["cell_widths"],
        "mu": quad_dict["mu"],
        # solution grid functions
        "psi_avg": psi_avg,
        "phi_avg": phi_avg,
        "phi_edge": phi_edge,
        "J_edge": J_edge,
        # QD closure and boundary factors
        "E_avg": E_avg,
        "E_edge": E_edge,
        "C_L": C_L,
        "C_R": C_R,
        # transport metrics
        "M_E": M_E,
        "M_E_prime": M_E_prime,
        # convergence
        "l_inf_history": np.array(l_inf_history),
        "rho_history": np.array(rho_history),
        "n_iterations": s,
    }

    return results_dict


def angular_mesh_sweep(problem, mesh_dict, quad_dict, phi_lld_prev):
    n_cells = mesh_dict["n_cells"]
    cell_widths = mesh_dict["cell_widths"]
    sigma_t = mesh_dict["sigma_t"]
    sigma_s = mesh_dict["sigma_s"]
    q = mesh_dict["q"]

    mu_arr = quad_dict["mu"]
    w_arr  = quad_dict["w"]

    phi_L = phi_lld_prev[:, 0]
    phi_R = phi_lld_prev[:, 1]

    n_angles = len(mu_arr)
    psi_avg  = np.zeros((n_angles, n_cells))
    psi_edge = np.zeros((n_angles, n_cells + 1))

    for m_idx, mu in enumerate(mu_arr):
        w = w_arr[m_idx]

        # forward sweep using positive half-range equations
        if mu > 0:
            if problem.left_bc == "vacuum":
                psi_incoming = 0.0
            if problem.left_bc == "incident":
                psi_incoming = problem.left_incident_flux

            psi_edge[m_idx, 0] = psi_incoming

            for cell_idx in range(n_cells):
                dx = cell_widths[cell_idx]

                # forcing matrix
                a11 = 0.5 * (mu + sigma_t[cell_idx] * dx)
                a12 = 0.5 * (mu)
                a21 = 0.5 * (-mu)
                a22 = 0.5 * (mu + sigma_t[cell_idx] * dx)

                # source vector
                f1 = dx/4 * (sigma_s[cell_idx] * phi_L[cell_idx] + q[cell_idx]) + mu * psi_incoming
                f2 = dx/4 * (sigma_s[cell_idx] * phi_R[cell_idx] + q[cell_idx])

                # manually computer inverse
                det = a11 * a22 - a12 * a21
                psi_L = (f1 * a22 - f2 * a12) / det
                psi_R = (f2 * a11 - f1 * a21) / det

                psi_avg[m_idx, cell_idx] = 0.5 * (psi_L + psi_R)
                psi_edge[m_idx, cell_idx + 1] = psi_R

                # incoming flux for next cell
                psi_incoming = psi_R
            continue

        # backward sweep using negative half-range equations
        if mu < 0:
            if problem.right_bc == "vacuum":
                psi_incoming = 0.0
            if problem.right_bc == "incident":
                psi_incoming = problem.right_incident_flux

            psi_edge[m_idx, n_cells] = psi_incoming

            for cell_idx in range(n_cells - 1, -1, -1):
                dx = cell_widths[cell_idx]

                # forcing matrix
                a11 = 0.5 * (-mu + sigma_t[cell_idx] * dx)
                a12 = 0.5 * (mu)
                a21 = 0.5 * (-mu)
                a22 = 0.5 * (-mu + sigma_t[cell_idx] * dx)

                # source vector
                f1 = dx/4 * (sigma_s[cell_idx] * phi_L[cell_idx] + q[cell_idx])
                f2 = dx/4 * (sigma_s[cell_idx] * phi_R[cell_idx] + q[cell_idx]) - mu * psi_incoming

                # manually computer inverse
                det = a11 * a22 - a12 * a21
                psi_L = (f1 * a22 - f2 * a12) / det
                psi_R = (f2 * a11 - f1 * a21) / det

                psi_avg[m_idx, cell_idx] = 0.5 * (psi_L + psi_R)
                psi_edge[m_idx, cell_idx] = psi_L

                # incoming flux for next cell
                psi_incoming = psi_L
            continue

    return psi_avg, psi_edge


def fv_loqd_solver(problem, mesh_dict, quad_dict, E_avg, E_edge, C_L, C_R):
    n_cells = mesh_dict["n_cells"]
    dx      = mesh_dict["cell_widths"]
    sigma_t = mesh_dict["sigma_t"]
    sigma_s = mesh_dict["sigma_s"]
    q       = mesh_dict["q"]
    sigma_a = sigma_t - sigma_s

    mu = quad_dict["mu"]
    w  = quad_dict["w"]

    # compute incoming partial fluxes and currents
    pos_mask = mu > 0
    neg_mask = mu < 0

    if problem.left_bc == "vacuum":
        phi_in_pos = 0.0
        J_in_pos   = 0.0
    if problem.left_bc == "incident":
        psi_L_in   = problem.left_incident_flux
        phi_in_pos = np.sum(psi_L_in * w[pos_mask])
        J_in_pos   = np.sum(mu[pos_mask] * psi_L_in * w[pos_mask])

    if problem.right_bc == "vacuum":
        phi_in_neg = 0.0
        J_in_neg   = 0.0
    elif problem.right_bc == "incident":
        psi_R_in   = problem.right_incident_flux
        phi_in_neg = np.sum(psi_R_in * w[neg_mask])
        J_in_neg   = np.sum(mu[neg_mask] * psi_R_in * w[neg_mask])

    # assemble matrix A
    a_banded = np.zeros((3, n_cells))
    f = q * dx

    tau = sigma_t * dx
    denom_adjacent = tau[:-1] + tau[1:]

    # off-diagonals
    a_banded[0, 1:]  = -2.0 * E_avg[1:] / denom_adjacent   # super-diag
    a_banded[2, :-1] = -2.0 * E_avg[:-1] / denom_adjacent  # sub-diag

    # main diagonal
    a_banded[1, 1:-1] = (
        2.0 * E_avg[1:-1] / denom_adjacent[:-1]
        + sigma_a[1:-1] * dx[1:-1]
        + 2.0 * E_avg[1:-1] / denom_adjacent[1:]
    )

    # left BC
    denom_L = E_edge[0] - 0.5 * tau[0] * C_L
    a_banded[1, 0] = (2.0 * E_avg[0] / denom_adjacent[0]) + (C_L * E_avg[0] / denom_L) + sigma_a[0] * dx[0]
    f[0] = q[0] * dx[0] + (E_edge[0] * (J_in_pos - C_L * phi_in_pos)) / denom_L

    # right BC
    denom_R = E_edge[-1] + 0.5 * tau[-1] * C_R
    a_banded[1, -1] = (2.0 * E_avg[-1] / denom_adjacent[-1]) + (C_R * E_avg[-1] / denom_R) + sigma_a[-1] * dx[-1]
    f[-1] = q[-1] * dx[-1] - (E_edge[-1] * (J_in_neg - C_R * phi_in_neg)) / denom_R

    phi_avg = solve_banded((1, 1), a_banded, f)

    # edge quantities
    phi_edge = np.zeros(n_cells + 1)
    J_edge = np.zeros(n_cells + 1)

    # interior cells
    phi_edge[1:-1] = (
        tau[1:] * E_avg[:-1] * phi_avg[:-1] + tau[:-1] * E_avg[1:] * phi_avg[1:]
    ) / (E_edge[1:-1] * denom_adjacent)

    J_edge[1:-1] = (2.0 / denom_adjacent) * (E_avg[:-1] * phi_avg[:-1] - E_avg[1:] * phi_avg[1:])

    # left bc
    phi_edge[0] = (E_avg[0] * phi_avg[0] + 0.5 * tau[0] * (J_in_pos - C_L * phi_in_pos)) / denom_L
    J_edge[0]   = (C_L * E_avg[0] * phi_avg[0] + E_edge[0] * (J_in_pos - C_L * phi_in_pos)) / denom_L

    # right bc
    phi_edge[-1] = (E_avg[-1] * phi_avg[-1] - 0.5 * tau[-1] * (J_in_neg - C_R * phi_in_neg)) / denom_R
    J_edge[-1]   = (C_R * E_avg[-1] * phi_avg[-1] + E_edge[-1] * (J_in_neg - C_R * phi_in_neg)) / denom_R

    return phi_avg, phi_edge, J_edge


def compute_qd_factors(quad_dict, psi_avg, psi_edge):
    mu = quad_dict["mu"]
    w  = quad_dict["w"]

    pos_mask = mu > 0
    neg_mask = mu < 0

    w_col  = w[:, None]
    mu_col = mu[:, None]

    # QD factors
    E_avg  = np.sum((mu_col ** 2) * psi_avg * w_col, axis=0) / np.sum(psi_avg * w_col, axis=0)
    E_edge = np.sum((mu_col ** 2) * psi_edge * w_col, axis=0) / np.sum(psi_edge * w_col, axis=0)

    # boundary factors
    C_L = np.sum(mu[neg_mask] * psi_edge[neg_mask, 0] * w[neg_mask]) / np.sum(psi_edge[neg_mask, 0] * w[neg_mask])
    C_R = np.sum(mu[pos_mask] * psi_edge[pos_mask, -1] * w[pos_mask]) / np.sum(psi_edge[pos_mask, -1] * w[pos_mask])

    return E_avg, E_edge, C_L, C_R
