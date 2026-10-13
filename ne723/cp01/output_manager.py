from pathlib import Path
import numpy as np
import time


def log_msg(text, file_obj=None):
    print(text)
    if file_obj is not None: file_obj.write(text + "\n")


def log_time(start_time, message, file_obj=None):
    elapsed_time = time.time() - start_time
    full_message = f"{message} (Elapsed Time: {elapsed_time:.4f} seconds)"
    log_msg(full_message, file_obj)


def print_problem_setup(problem, file_obj=None):
    lines = [
        "=" * 50,
        f"INITIALIZING SOLVER FOR '{problem.problem_name}'",
        "=" * 50,
        f"  Problem Type:              {problem.problem_type}",
        f"  Convergence Criteria:      {problem.convergence_criteria}",
        f"  Uniform Spatial Mesh:      {problem.uniform_dx}",
        f"    Number of Mesh Cells:    {problem.n_mesh_cells}",
        f"  Legendre Order:            {problem.legendre_order}",
        f"  Left BC:                   {problem.left_bc}",
    ]

    if hasattr(problem, "left_incident_flux"):
        lines.append(f"    Left Incident Flux:      {problem.left_incident_flux}")

    lines.extend(
        [
            f"  Right BC:                  {problem.right_bc}",
            f"  Region Boundaries:         {problem.region_boundaries}",
            f"  Total Cross Sections:      {problem.sigma_ts}",
            f"  Scattering Cross Sections: {problem.sigma_ss}",
            f"  Fission Cross Sections:    {problem.sigma_fs}",
            f"  Nu-Fission:                {problem.nu_fs}",
            f"  External Source:           {problem.qs}",
            "=" * 50,
        ]
    )

    for line in lines:
        log_msg(line, file_obj)


def save_results(results_dir, problem, mesh_dict, results_dict):
    """ save all results """
    problem_name = problem.problem_name
    problem_type = problem.problem_type

    cell_centers, phi_bar, phi_hat = save_cell_results(
        results_dir, problem_name, mesh_dict, results_dict
    )
    cell_edges, phi_edges, j_edges = save_edge_results(
        results_dir, problem_name, mesh_dict, results_dict
    )

    psi_matrix = save_angular_flux_results(results_dir, problem_name, mesh_dict, results_dict)

    if problem_type == "fixed_source":
        history = save_history_results(results_dir, problem_name, results_dict, problem_type)

    if problem_type == "k-eigenvalue":
        inner_history, outer_history = save_history_results(results_dir, problem_name, results_dict, problem_type)


    """
    import plots

    plots.generate_report_plots(
        results_dir=results_dir,
        problem=problem,
        x_centers=x_centers,
        phi_bar=phi_bar,
        phi_hat=phi_hat,
        cell_edges_x=cell_edges_x,
        phi_edges=phi_edges,
        j_edges=j_edges,
        results_dict=results_dict,
    )
    """


def save_cell_results(results_dir, problem_name, mesh_dict, results_dict):
    dx = mesh_dict["dx"]
    cell_edges = mesh_dict["cell_edges"]
    n_cells = len(dx)

    phi_L = results_dict["phi_L"]
    phi_R = results_dict["phi_R"]
    dr_0 = results_dict["delta_r_0"]
    dr_1 = results_dict["delta_r_1"]
    dR_0 = results_dict["delta_R_0"]
    dR_1 = results_dict["delta_R_1"]

    cell_centers = np.zeros(n_cells)
    phi_bar = np.zeros(n_cells)
    phi_hat = np.zeros(n_cells)

    out_file = results_dir / f"{problem_name}_cell_moments_and_residuals.txt"
    with open(out_file, "w") as f:
        f.write(f"CELL CENTERED MOMENTS AND RESIDUALS for {problem_name}\n")

        f.write(
            f"{'Cell idx':<8s}{'x_i (cm)':<12s}"
            f"{'phi_bar_i':<14s}{'phi_hat_i':<14s}"
            f"{'dr_0':<14s}{'dr_1':<14s}"
            f"{'dR_0':<14s}{'dR_1':<14s}\n"
        )

        for cell_idx in range(n_cells):
            cell_centers[cell_idx] = cell_edges[cell_idx] + 0.5 * dx[cell_idx]
            phi_bar[cell_idx] = 0.5 * (phi_R[cell_idx] + phi_L[cell_idx])
            phi_hat[cell_idx] = 0.5 * (phi_R[cell_idx] - phi_L[cell_idx])

            f.write(
                f"{cell_idx:<8d}{cell_centers[cell_idx]:<12.5f}{phi_bar[cell_idx]:<14.6e}{phi_hat[cell_idx]:<14.6e}"
                f"{dr_0[cell_idx]:<14.6e}{dr_1[cell_idx]:<14.6e}{dR_0[cell_idx]:<14.6e}{dR_1[cell_idx]:<14.6e}\n"
            )

    return cell_centers, phi_bar, phi_hat


def save_edge_results(results_dir, problem_name, mesh_dict, results_dict):
    cell_edges = mesh_dict["cell_edges"]
    n_cells = len(mesh_dict["dx"])

    phi_L = results_dict["phi_L"]
    phi_R = results_dict["phi_R"]
    j_L = results_dict["j_L"]
    j_R = results_dict["j_R"]

    phi_edges = np.zeros(n_cells + 1)
    j_edges = np.zeros(n_cells + 1)

    out_file = results_dir / f"{problem_name}_cell_edge_flux_and_current.txt"
    with open(out_file, "w") as f:
        f.write(f"CELL EDGE INTERFACE QUANTITIES for {problem_name}\n")
        f.write(
            f"{'Edge idx':<12s}{'x_i+1/2 (cm)':<12s}"
            f"{'phi_edge':<18s}{'j_edge':<18s}\n"
        )

        # left edge
        phi_edges[0] = phi_L[0]
        j_edges[0] = j_L[0]
        f.write(
            f"{0:<12d}{cell_edges[0]:<12.5f}{phi_edges[0]:<18.6e}{j_edges[0]:<18.6e}\n"
        )

        # internal
        for cell_idx in range(n_cells - 1):
            phi_edges[cell_idx + 1] = 0.5 * (phi_R[cell_idx] + phi_L[cell_idx + 1])
            j_edges[cell_idx + 1] = 0.5 * (j_R[cell_idx] + j_L[cell_idx + 1])
            f.write(
                f"{cell_idx+1:<12d}{cell_edges[cell_idx + 1]:<12.5f}{phi_edges[cell_idx +1]:<18.6e}{j_edges[cell_idx + 1]:<18.6e}\n"
            )

        # right edge
        phi_edges[-1] = phi_R[-1]
        j_edges[-1] = j_R[-1]
        f.write(
            f"{n_cells:<12d}{cell_edges[-1]:<12.5f}{phi_edges[-1]:<18.6e}{j_edges[-1]:<18.6e}\n"
        )

    return cell_edges, phi_edges, j_edges


def save_angular_flux_results(results_dir, problem_name, mesh_dict, results_dict):
    dx = mesh_dict["dx"]
    cell_edges = mesh_dict["cell_edges"]
    n_cells = len(dx)

    psi_matrix = results_dict["psi_0moment_matrix"]
    n_angles = psi_matrix.shape[0]

    out_file = results_dir / f"{problem_name}_angular_flux_matrix.txt"
    with open(out_file, "w") as f:
        f.write(f"2D CELL-AVERAGE ANGULAR FLUX MATRIX psi_bar(x_i, mu_m) for {problem_name}\n")
        f.write(f"Rows: Angle indices (angle_idx) | Columns: Cell indices (cell_idx)\n")

        f.write(f"{'Angle idx':<12s}")
        for cell_idx in range(n_cells):
            x_i = cell_edges[cell_idx] + 0.5 * dx[cell_idx]
            f.write(f"| x={x_i:.3f}  ")
        f.write("\n" + "-" * 100 + "\n")

        for angle_idx in range(n_angles):
            f.write(f"Angle {angle_idx:<6d}")
            for cell_idx in range(n_cells):
                f.write(f"  {psi_matrix[angle_idx, cell_idx]:.4e}")
            f.write("\n")

    return psi_matrix


def save_history_results(results_dir, problem_name, results_dict, problem_type):
    """Saves the step-by-step solver tracking convergence history log fields."""
    out_file = results_dir / f"{problem_name}_convergence_history.txt"

    with open(out_file, "w") as f:
        if problem_type == "fixed_source":
            history = results_dict["iteration_history"]
            f.write(f"INNER ITERATION CONVERGENCE HISTORY LOG for {problem_name}\n")
            f.write(f"Total Source Iterations (N_iter) completed: {len(history)}\n")
            f.write(f"{'Iteration idx':<15s}{'Max-Norm Error':<25s}{'Spectral Radius (rho)':<25s}\n")

            for row_idx in range(len(history)):
                row_data = history[row_idx]
                rho_val = row_data["rho"]
                rho_str = f"{rho_val:.5f}" if rho_val is not None else "None"
                f.write(f"{row_data['iteration']:<15d}{row_data['error']:<25.5e}{rho_str:<25s}\n")

            return history

        if problem_type == "k-eigenvalue":
            inner_history = results_dict["inner_history"]
            outer_history = results_dict["outer_history"]

            f.write(f"CRITICALITY SEARCH ITERATION HISTORIES LOG for {problem_name}\n\n")
            f.write("1. Outer loop: secant method geometry search widths\n")
            f.write(f"{'Outer Step idx':<18s}{'Slab Width a_crit (cm)':<25s}\n")

            for outer_idx in range(len(outer_history)):
                outer_row = outer_history[outer_idx]
                f.write(f"{outer_row['iteration']:<18d}{outer_row['a_crit']:<25.5f}\n")

            f.write("\n2. Inner power iteration balance track (from final iteration)\n")
            f.write(f"{'Inner Step idx':<16s}{'Error (phi)':<20s}{'Error (k)':<20s}{'k_effective':<18s}\n")

            for inner_idx in range(len(inner_history)):
                inner_row = inner_history[inner_idx]
                f.write(
                    f"{inner_row['iteration']:<16d}"
                    f"{inner_row['error_phi']:<20.5e}"
                    f"{inner_row['error_k']:<20.5e}"
                    f"{inner_row['k_eff']:<18.6f}\n"
                )

            return inner_history, outer_history
