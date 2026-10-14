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
    problem_name = problem.problem_name
    problem_type = problem.problem_type

    save_cell_centered_moments_and_residuals(results_dir, problem_name, mesh_dict, results_dict)
    save_cell_edge_flux_and_current(results_dir, problem_name, mesh_dict, results_dict)
    save_angular_flux_moments(results_dir, problem_name, mesh_dict, results_dict)
    save_cell_edge_angular_flux(results_dir, problem_name, mesh_dict, results_dict)

    save_history_results(results_dir, problem_name, results_dict, problem_type)


def save_cell_centered_moments_and_residuals(results_dir, problem_name, mesh_dict, results_dict):
    dx = mesh_dict["dx"]
    cell_edges = mesh_dict["cell_edges"]
    n_cells = len(dx)

    phi_bar = results_dict["cell_average_scalar_flux"]
    phi_hat = results_dict["first_legendre_moment_scalar_flux"]
    j_bar = results_dict["cell_average_current"]
    j_hat = results_dict["first_legendre_moment_current"]
    dr_0 = results_dict["delta_r_0"]
    dr_1 = results_dict["delta_r_1"]
    dR_0 = results_dict["delta_R_0"]
    dR_1 = results_dict["delta_R_1"]

    out_file = results_dir / f"{problem_name}_cell_moments_and_residuals.txt"
    with open(out_file, "w") as f:
        f.write(f"CELL CENTERED MOMENTS AND RESIDUALS for {problem_name}\n")
        f.write(
            f"{'Cell idx':<10s}{'x_i (cm)':<12s}"
            f"{'phi_bar_i':<14s}{'phi_hat_i':<14s}"
            f"{'j_bar_i':<14s}{'j_hat_i':<14s}"
            f"{'dr_0':<14s}{'dr_1':<14s}"
            f"{'dR_0':<14s}{'dR_1':<14s}\n"
        )

        for cell_idx in range(n_cells):
            cell_center = cell_edges[cell_idx] + 0.5 * dx[cell_idx]
            f.write(
                f"{cell_idx:<10d}{cell_center:<12.5f}"
                f"{phi_bar[cell_idx]:<14.6e}{phi_hat[cell_idx]:<14.6e}"
                f"{j_bar[cell_idx]:<14.6e}{j_hat[cell_idx]:<14.6e}"
                f"{dr_0[cell_idx]:<14.6e}{dr_1[cell_idx]:<14.6e}"
                f"{dR_0[cell_idx]:<14.6e}{dR_1[cell_idx]:<14.6e}\n"
            )


def save_cell_edge_flux_and_current(results_dir, problem_name, mesh_dict, results_dict):
    cell_edges = mesh_dict["cell_edges"]
    n_cells = len(mesh_dict["dx"])

    phi_edges = results_dict["cell_edge_scalar_flux"]
    j_edges = results_dict["cell_edge_current"]

    out_file = results_dir / f"{problem_name}_cell_edge_flux_and_current.txt"
    with open(out_file, "w") as f:
        f.write(f"CELL EDGE QUANTITIES for {problem_name}\n")
        f.write(
            f"{'Edge idx':<12s}{'x_i+1/2 (cm)':<15s}"
            f"{'phi_edge':<20s}{'j_edge':<20s}\n"
        )

        for edge_idx in range(n_cells + 1):
            f.write(
                f"{edge_idx:<12d}{cell_edges[edge_idx]:<15.5f}"
                f"{phi_edges[edge_idx]:<20.6e}{j_edges[edge_idx]:<20.6e}\n"
            )


def save_angular_flux_moments(results_dir, problem_name, mesh_dict, results_dict):
    dx = mesh_dict["dx"]
    cell_edges = mesh_dict["cell_edges"]
    n_cells = len(dx)

    psi_0moment = results_dict["cell_average_angular_flux"]
    psi_1moment = results_dict["first_legendre_moment_angular_flux"]
    n_angles = psi_0moment.shape[0]

    out_file = results_dir / f"{problem_name}_angular_flux_moments.txt"
    with open(out_file, "w") as f:
        f.write(f"2D ANGULAR FLUX MOMENTS FOR {problem_name}\n")
        f.write(f"Format for each entry: psi_bar / psi_hat\n")
        f.write(f"{'Angle idx':<12s}")
        for cell_idx in range(n_cells):
            x_i = cell_edges[cell_idx] + 0.5 * dx[cell_idx]
            f.write(f"| x={x_i:.3f}          ")
        f.write("\n" + "-" * (14 + 22 * n_cells) + "\n")

        for angle_idx in range(n_angles):
            f.write(f"Angle {angle_idx:<6d}")
            for cell_idx in range(n_cells):
                val_bar = psi_0moment[angle_idx, cell_idx]
                val_hat = psi_1moment[angle_idx, cell_idx]
                f.write(f"  {val_bar:.3e}/{val_hat:+.3e}")
            f.write("\n")


def save_cell_edge_angular_flux(results_dir, problem_name, mesh_dict, results_dict):
    cell_edges = mesh_dict["cell_edges"]
    n_cells = len(mesh_dict["dx"])

    psi_edge = results_dict["cell_edge_angular_flux"]
    n_angles = psi_edge.shape[0]

    out_file = results_dir / f"{problem_name}_cell_edge_angular_flux.txt"
    with open(out_file, "w") as f:
        f.write(f"2D CELL EDGE ANGULAR FLUX INTERFACES FOR {problem_name}\n")
        f.write(f"{'Angle idx':<12s}")
        for edge_idx in range(n_cells + 1):
            f.write(f"| x_e={cell_edges[edge_idx]:.3f}    ")
        f.write("\n" + "-" * (14 + 18 * (n_cells + 1)) + "\n")

        for angle_idx in range(n_angles):
            f.write(f"Angle {angle_idx:<6d}")
            for edge_idx in range(n_cells + 1):
                f.write(f"  {psi_edge[angle_idx, edge_idx]:.4e}")
            f.write("\n")


def save_history_results(results_dir, problem_name, results_dict, problem_type):
    out_file = results_dir / f"{problem_name}_convergence_history.txt"

    with open(out_file, "w") as f:
        if problem_type == "fixed_source":
            history = results_dict["iteration_history"]
            f.write(f"INNER ITERATION CONVERGENCE HISTORY for {problem_name}\n")
            f.write(f"Total Source Iterations (N_iter) completed: {len(history)}\n")
            f.write(f"{'Iteration idx':<15s}{'Max-Norm Error':<25s}{'Spectral Radius (rho)':<25s}\n")

            for row_idx in range(len(history)):
                row_data = history[row_idx]
                rho_val = row_data["rho"]
                rho_str = f"{rho_val:.5f}" if rho_val is not None else "None"
                f.write(f"{row_data['iteration']:<15d}{row_data['error']:<25.5e}{rho_str:<25s}\n")

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
