import numpy as np
import time

import mesh_builder
import output_manager


def fixed_source_solver(mesh_dict, quad_dict, problem, k_eff=1.0, file_obj=None):
    """used to solve tests a and b"""
    epsilon = problem.convergence_criteria

    dx = mesh_dict["dx"]
    cell_edges = mesh_dict["cell_edges"]
    sigma_t = mesh_dict["sigma_t"]
    sigma_s = mesh_dict["sigma_s"]
    sigma_f = mesh_dict["sigma_f"]
    nu_f = mesh_dict["nu_f"]
    q = mesh_dict["q"]

    n_cells = len(dx)
    phi_L = np.zeros(n_cells)
    phi_R = np.zeros(n_cells)

    converged = False
    error_previous = 0.0
    iteration_idx = 0
    iteration_history = []

    psi_left_reflective = None


    # precompute static "matrices"
    # streaming & BC terms are constants

    # collision
    sigma_t_dx_over_3 = (1 / 3) * sigma_t * dx
    sigma_t_dx_over_6 = (1 / 6) * sigma_t * dx

    # scattering and fission source
    production_coeff = sigma_s + (sigma_f * nu_f / k_eff)
    internal_source_diag = (1 / 6) * production_coeff * dx
    internal_source_off = (1 / 12) * production_coeff * dx

    # external source
    external_source = 0.25 * q * dx

    # residual placeholders
    delta_r_0, delta_r_1, delta_R_0, delta_R_1 = None, None, None, None

    loop_start_time = time.time()

    while not converged:
        iteration_idx += 1

        phi_L_prev = phi_L.copy()
        phi_R_prev = phi_R.copy()

        b_source_L = (internal_source_diag * phi_L_prev + internal_source_off * phi_R_prev) + external_source
        b_source_R = (internal_source_off * phi_L_prev + internal_source_diag * phi_R_prev) + external_source

        phi_L, phi_R, j_L, j_R, psi_left_reflective, psi_0moment_matrix = angular_mesh_sweep(
            mesh_dict, quad_dict,
            sigma_t_dx_over_3, sigma_t_dx_over_6,
            b_source_L, b_source_R,
            problem, psi_left_reflective
        )

        error_L = np.max(np.abs(phi_L - phi_L_prev))
        error_R = np.max(np.abs(phi_R - phi_R_prev))
        error_current = max(error_L, error_R)

        rho = None

        if iteration_idx >= 2 and error_previous > 0.0:
            rho = error_current / error_previous

            if 0.0 < rho < 1.0:
                threshold = epsilon * ((1 / rho) - 1.0)
                if error_current < threshold:
                    converged = True

        # calculate residuals once converged
        if converged:
            phi_0moment_current = 0.5 * (phi_L + phi_R)
            phi_1moment_current = 0.5 * (phi_R - phi_L)

            phi_0moment_prev = 0.5 * (phi_L_prev + phi_R_prev)
            phi_1moment_prev = 0.5 * (phi_R_prev - phi_L_prev)

            q_0moment_integrated = q * dx
            q_1moment_integrated = np.zeros(n_cells)

            production = sigma_s + (sigma_f * nu_f / 1)

            # scheme residual, particle balance (Eq. 8a)
            delta_r_0 = (j_R - j_L) + (sigma_t * phi_0moment_current * dx) - (production * phi_0moment_prev * dx) - q_0moment_integrated

            # scheme residual, first moment (Eq. 8b)
            j_0moment = 0.5 * (j_L + j_R)
            delta_r_1 = 3.0 * (j_R + j_L - 2 * j_0moment) + (sigma_t * phi_1moment_current * dx) - (production * phi_1moment_prev * dx) - q_1moment_integrated

            # iterative residual, particle palance (Eq. 9a)
            delta_R_0 = (j_R - j_L) + ((sigma_t - production) * phi_0moment_current * dx) - q_0moment_integrated

            # iterative residual, first moment (Eq. 9b)
            delta_R_1 = 3.0 * (j_R + j_L - 2 * j_0moment) + ((sigma_t - production) * phi_1moment_current * dx) - q_1moment_integrated


        iteration_history.append({
            "iteration": iteration_idx,
            "error": error_current,
            "rho": rho,
        })

        if iteration_idx % 10 == 0:
            elapsed = time.time() - loop_start_time
            msg = f"  > Iteration {iteration_idx:<5d} | Current Error: {error_current:.4e} | Elapsed Time: {elapsed:.3f} s"
            output_manager.log_msg(msg, file_obj)

        error_previous = error_current

    results_dict = {
        "phi_L": phi_L,
        "phi_R": phi_R,
        "j_L": j_L,
        "j_R": j_R,
        "psi_0moment_matrix": psi_0moment_matrix,
        "iteration_history": iteration_history,

        "delta_r_0": delta_r_0,
        "delta_r_1": delta_r_1,
        "delta_R_0": delta_R_0,
        "delta_R_1": delta_R_1,
    }

    return results_dict


def angular_mesh_sweep(
    mesh_dict, quad_dict,
    sigma_t_dx_over_3, sigma_t_dx_over_6,
    b_source_L, b_source_R,
    problem, psi_left_reflective
):
    dx = mesh_dict["dx"]
    mu_arr = quad_dict["mu"]
    w_arr = quad_dict["w"]
    n_cells = len(dx)
    n_angles = len(mu_arr)

    phi_L_new = np.zeros(n_cells)
    phi_R_new = np.zeros(n_cells)
    j_L_new = np.zeros(n_cells)
    j_R_new = np.zeros(n_cells)

    psi_0moment_matrix = np.zeros((n_angles, n_cells))

    if psi_left_reflective is None:
        psi_left_reflective = np.zeros(n_angles)

    for m_idx, mu in enumerate(mu_arr):
        w = w_arr[m_idx]

        # sweep to the right (forward)
        if mu > 0:
            if problem.left_bc == "incident":
                psi_incoming = problem.left_incident_flux
            if problem.left_bc == "reflective":
                psi_incoming = psi_left_reflective[m_idx]
            if problem.left_bc == "vacuum":
                psi_incoming = 0.0

            for cell_idx in range(n_cells):
                # streaming w/o incoming flux + total loss
                a11 = 0.5 * mu + sigma_t_dx_over_3[cell_idx]
                a12 = 0.5 * mu + sigma_t_dx_over_6[cell_idx]
                a21 = -0.5 * mu + sigma_t_dx_over_6[cell_idx]
                a22 = -0.5 * mu + sigma_t_dx_over_3[cell_idx]

                # sources + incoming flux
                b1 = b_source_L[cell_idx] + mu * psi_incoming
                b2 = b_source_R[cell_idx]

                # calc det for analytical inversion
                det = a11 * a22 - a12 * a21
                psi_L = (b1 * a22 - b2 * a12) / det
                psi_R = (a11 * b2 - a21 * b1) / det

                psi_0moment_matrix[m_idx, cell_idx] = 0.5 * (psi_L + psi_R)

                # add flux contribution
                phi_L_new[cell_idx] += w * psi_L
                phi_R_new[cell_idx] += w * psi_R
                j_L_new[cell_idx] += w * mu * psi_L
                j_R_new[cell_idx] += w * mu * psi_R

                # set incoming flux for next cell
                psi_incoming = psi_R
            continue

        # sweep to the left (backward)
        if mu < 0:
            if problem.right_bc == "vacuum":
                psi_incoming = 0.0
            if problem.right_bc != "vacuum":
                raise ValueError("Support for non-vacuum BC not added for right")

            for cell_idx in range(n_cells - 1, -1, -1):
                # streaming w/o incoming flux + total loss
                a11 = 0.5 * mu + sigma_t_dx_over_3[cell_idx]
                a12 = 0.5 * mu + sigma_t_dx_over_6[cell_idx]
                a21 = -0.5 * mu + sigma_t_dx_over_6[cell_idx]
                a22 = -0.5 * mu + sigma_t_dx_over_3[cell_idx]

                # sources - incoming flux
                b1 = b_source_L[cell_idx]
                b2 = b_source_R[cell_idx] - mu * psi_incoming

                # calc det for analytical inversion
                det = a11 * a22 - a12 * a21
                psi_L = (b1 * a22 - b2 * a12) / det
                psi_R = (a11 * b2 - a21 * b1) / det

                psi_0moment_matrix[m_idx, cell_idx] = 0.5 * (psi_L + psi_R)

                # add flux contribution
                phi_L_new[cell_idx] += w * psi_L
                phi_R_new[cell_idx] += w * psi_R
                j_L_new[cell_idx] += w * mu * psi_L
                j_R_new[cell_idx] += w * mu * psi_R

                # set incoming flux for next cell
                psi_incoming = psi_L

            if problem.left_bc == "reflective":
                mirrored_idx = n_angles - m_idx - 1
                psi_left_reflective[mirrored_idx] = psi_L

            continue

    return phi_L_new, phi_R_new, j_L_new, j_R_new, psi_left_reflective, psi_0moment_matrix


def critical_width_solver(quad_dict, problem, file_obj=None):
    """used to solve test c"""
    outer_history = []

    a_0 = problem.region_boundaries[-1]
    a_1 = a_0 * 1.15

    k_0, _, _, _, _, _, _, _, _, _, _ = eigenvalue_inner_loop(a_0, quad_dict, problem, file_obj)
    msg = f"Initial Guess 1: width = {a_0:.4f} cm; k_eff = {k_0:.6f}"
    output_manager.log_msg(msg, file_obj)

    k_1, _, _, _, _, _, _, _, _, _, _ = eigenvalue_inner_loop(a_1, quad_dict, problem, file_obj)
    msg = f"Initial Guess 2: width = {a_1:.4f} cm; k_eff = {k_1:.6f}"
    output_manager.log_msg(msg, file_obj)

    outer_converged = False
    outer_idx = 0
    max_outer_iters = 50

    phi_L, phi_R, j_L, j_R, history, psi_0moment_matrix = None, None, None, None, None, None
    delta_r_0, delta_r_1, delta_R_0, delta_R_1 = None, None, None, None

    while not outer_converged and outer_idx < max_outer_iters:
        outer_idx += 1

        # use secant method
        a_next = a_1 - (k_1 - 1.0) * (a_1 - a_0) / (k_1 - k_0)

        (
            k_next, phi_L, phi_R, j_L, j_R, history, psi_0moment_matrix,
            delta_r_0, delta_r_1, delta_R_0, delta_R_1,
        )
            = eigenvalue_inner_loop(
            a_next, quad_dict, problem, file_obj
        )

        outer_history.append({"iteration": outer_idx, "a_crit": a_next})

        msg = f"Outer Step {outer_idx:<2d}: width = {a_next:.5f} cm; k_eff = {k_next:.6f}"
        output_manager.log_msg(msg, file_obj)

        if abs(k_next - 1.0) < 1e-5:
            msg = f"!!! Critical width found (k=1), a = {a_next:.5f} cm\n !!!"
            output_manager.log_msg(msg, file_obj)
            outer_converged = True

        a_0 = a_1
        a_1 = a_next
        k_0 = k_1
        k_1 = k_next

    results_dict = {
        "critical_width": a_next,
        "phi_L": phi_L,
        "phi_R": phi_R,
        "j_L": j_L,
        "j_R": j_R,
        "psi_0moment_matrix": psi_0moment_matrix,
        "inner_history": history,
        "outer_history": outer_history,

        "delta_r_0": delta_r_0,
        "delta_r_1": delta_r_1,
        "delta_R_0": delta_R_0,
        "delta_R_1": delta_R_1,
    }

    return results_dict


def eigenvalue_inner_loop(width_guess, quad_dict, problem, file_obj=None):
    """ inner loop solver for the k-eigenvalue solver """
    problem.region_boundaries = [0, width_guess]
    mesh_dict = mesh_builder.discretize_space(problem)

    dx = mesh_dict["dx"]
    sigma_t = mesh_dict["sigma_t"]
    sigma_s = mesh_dict["sigma_s"]
    sigma_f = mesh_dict["sigma_f"]
    nu_f = mesh_dict["nu_f"]
    q = mesh_dict["q"]

    n_cells = len(dx)
    phi_L = np.ones(n_cells)
    phi_R = np.ones(n_cells)

    epsilon = problem.convergence_criteria

    k_current = 1.0
    k_prev1 = 1.0
    k_prev2 = 1.0

    inner_converged = False
    iteration_idx = 0
    iteration_history = []
    error_phi_previous = 0

    # residual placeholders
    delta_r_0, delta_r_1, delta_R_0, delta_R_1 = None, None, None, None

    psi_left_reflective = None

    while not inner_converged:
        iteration_idx += 1

        phi_L_prev = phi_L.copy()
        phi_R_prev = phi_R.copy()
        phi_0moment_prev = 0.5 * (phi_L_prev + phi_R_prev)

        # precompute static "matrices"
        # streaming & BC terms are constants

        # collision
        sigma_t_dx_over_3 = (1 / 3) * sigma_t * dx
        sigma_t_dx_over_6 = (1 / 6) * sigma_t * dx

        # scattering and fission source
        production_coeff = sigma_s + (sigma_f * nu_f / k_current)
        internal_source_diag = (1 / 6) * production_coeff * dx
        internal_source_off = (1 / 12) * production_coeff * dx

        # external source
        external_source = (1 / 4) * q * dx

        # total right side
        b_source_L = (internal_source_diag * phi_L_prev + internal_source_off * phi_R_prev) + external_source
        b_source_R = (internal_source_off * phi_L_prev + internal_source_diag * phi_R_prev) + external_source

        phi_L_new, phi_R_new, j_L, j_R, psi_left_reflective, psi_0moment_matrix = angular_mesh_sweep(
            mesh_dict, quad_dict,
            sigma_t_dx_over_3, sigma_t_dx_over_6,
            b_source_L, b_source_R,
            problem, psi_left_reflective
        )

        phi_0moment_new = 0.5 * (phi_L_new + phi_R_new)

        numer = np.sum(sigma_f * nu_f * phi_0moment_new * dx)
        denom = np.sum(sigma_f * nu_f * phi_0moment_prev * dx)
        k_next = k_current * (numer / denom)

        # convergence criteria
        error_phi_L = np.max(np.abs(phi_L_new - phi_L_prev))
        error_phi_R = np.max(np.abs(phi_R_new - phi_R_prev))
        error_phi_current = max(error_phi_L, error_phi_R)

        error_k_current = abs(k_next - k_current)

        converged_phi = False
        converged_k = False

        # flux shape spectral radius check
        if iteration_idx >= 2 and error_phi_previous > 0:
            rho_phi = error_phi_current / error_phi_previous
            if 0 < rho_phi < 1:
                threshold_phi = epsilon * ((1 / rho_phi) - 1)
                if error_phi_current < threshold_phi:
                    converged_phi = True

        # k-eigenvalue spectral radius check
        if iteration_idx >= 3:
            denom_k = abs(k_current - k_prev2)
            if denom_k > 0:
                rho_k = error_k_current / denom_k
                if 0 < rho_k < 1:
                    threshold_k = epsilon * ((1 / rho_k) - 1)
                    if error_k_current < threshold_k:
                        converged_k = True

        if converged_phi and converged_k:
            inner_converged = True

        if inner_converged:
            phi_0moment_current = 0.5 * (phi_L + phi_R)
            phi_1moment_current = 0.5 * (phi_R - phi_L)

            phi_0moment_prev = 0.5 * (phi_L_prev + phi_R_prev)
            phi_1moment_prev = 0.5 * (phi_R_prev - phi_L_prev)

            q_0moment_integrated = q * dx
            q_1moment_integrated = np.zeros(n_cells)

            production = sigma_s + (sigma_f * nu_f / k_next)

            # scheme residual, particle balance (Eq. 8a)
            delta_r_0 = (j_R - j_L) + (sigma_t * phi_0moment_current * dx) - (production * phi_0moment_prev * dx) - q_0moment_integrated

            # scheme residual, first moment (Eq. 8b)
            j_0moment = 0.5 * (j_L + j_R)
            delta_r_1 = 3.0 * (j_R + j_L - 2 * j_0moment) + (sigma_t * phi_1moment_current * dx) - (production * phi_1moment_prev * dx) - q_1moment_integrated

            # iterative residual, particle palance (Eq. 9a)
            delta_R_0 = (j_R - j_L) + ((sigma_t - production) * phi_0moment_current * dx) - q_0moment_integrated

            # iterative residual, first moment (Eq. 9b)
            delta_R_1 = 3.0 * (j_R + j_L - 2 * j_0moment) + ((sigma_t - production) * phi_1moment_current * dx) - q_1moment_integrated

        iteration_history.append({
            "iteration": iteration_idx,
            "error_phi": error_phi_current,
            "error_k": error_k_current,
            "k_eff": k_next
        })

        error_phi_previous = error_phi_current
        k_prev2 = k_prev1
        k_prev1 = k_current
        k_current = k_next
        phi_L, phi_R = phi_L_new, phi_R_new

    # normalizing
    phi_0moment = 0.5 * (phi_L + phi_R)
    phi_integral = np.sum(phi_0moment * dx)
    phi_L /= phi_integral
    phi_R /= phi_integral

    return k_current, phi_L, phi_R, j_L, j_R, iteration_history, psi_0moment_matrix, delta_r_0, delta_r_1, delta_R_0, delta_R_1
