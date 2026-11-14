from pathlib import Path
import numpy as np
import time


def print_problem_setup(problem, log_file):
    lines = [
        "=" * 50,
        f"INITIALIZING SOLVER FOR '{problem.problem_name}'",
        "=" * 50,
        f"  Region Boundaries:  {problem.region_boundaries}",
        f"  Total Cross Sections:  {problem.sigma_ts}",
        f"  Scattering Cross Sections:  {problem.sigma_ss}",
        f"  Fission Cross Sections:  {problem.sigma_fs}",
        f"  External Sources:  {problem.qs}",
        f"  Left Boundary Condition:  {problem.left_bc}",
    ]

    if hasattr(problem, "left_incident_flux"):
        msg = f"    Left Incident Flux:      {problem.left_incident_flux}"
        lines.append(msg)

    lines.extend([
        f"  Right Boundary Condition:  {problem.right_bc}",
        f"  Delta xs: {problem.delta_xs}",
        f"  Double Legendre Order:  {problem.double_legendre_order}",
        f"  Convergence Criteria:      {problem.convergence_criteria}",
    ])

    [log_msg(line, log_file) for line in lines]
    return


def log_msg(msg, file_obj):
    print(msg)
    file_obj.write(msg + "\n")
    return

def log_time(start_time, msg, file_obj):
    elapsed_time = time.time() - start_time
    full_msg = f"{msg} (Elapsed Time: {elapsed_time:.4f} seconds)"
    log_msg(full_msg, file_obj)
    return


def save_results(problem, results_dict, results_dir):
    problem_name = problem.problem_name

    # binary for --plot mode!!
    np.savez(results_dir / f"{problem_name}_results.npz", **results_dict)

    # saving formatted values
    save_cell_average_quantities(results_dir, problem_name, results_dict)
    save_cell_edge_quantities(results_dir, problem_name, results_dict)
    save_angular_flux(results_dir, problem_name, results_dict)
    save_convergence_summary(results_dir, problem_name, results_dict)


def save_cell_average_quantities(results_dir, problem_name, results_dict):
    x_i       = results_dict["cell_centers"]
    phi_avg   = results_dict["phi_avg"]
    E_avg     = results_dict["E_avg"]
    M_E       = results_dict["M_E"]
    M_E_prime = results_dict["M_E_prime"]

    out_file = results_dir / f"{problem_name}_cell_averages.txt"
    with open(out_file, "w") as f:
        f.write(f"CELL-AVERAGE QUANTITIES FOR {problem_name}\n")
        f.write(
            f"{'Cell idx':<10s}{'x_i (cm)':<12s}"
            f"{'phi_i':<16s}{'E_i':<16s}"
            f"{'M^E_i':<16s}{'M^E_prime_i':<16s}\n"
        )
        for i in range(len(x_i)):
            f.write(
                f"{i:<10d}{x_i[i]:<12.5f}"
                f"{phi_avg[i]:<16.6e}{E_avg[i]:<16.6e}"
                f"{M_E[i]:<16.6e}{M_E_prime[i]:<16.6e}\n"
            )


def save_cell_edge_quantities(results_dir, problem_name, results_dict):
    x_edge   = results_dict["cell_edges"]
    phi_edge = results_dict["phi_edge"]
    J_edge   = results_dict["J_edge"]
    E_edge   = results_dict["E_edge"]

    out_file = results_dir / f"{problem_name}_cell_edges.txt"
    with open(out_file, "w") as f:
        f.write(f"CELL-EDGE QUANTITIES FOR {problem_name}\n")
        f.write(
            f"{'Edge idx':<10s}{'x_i+1/2 (cm)':<14s}"
            f"{'phi_i+1/2':<16s}{'J_i+1/2':<16s}{'E_i+1/2':<16s}\n"
        )
        for i in range(len(x_edge)):
            f.write(
                f"{i:<10d}{x_edge[i]:<14.5f}"
                f"{phi_edge[i]:<16.6e}{J_edge[i]:<16.6e}{E_edge[i]:<16.6e}\n"
            )


def save_angular_flux(results_dir, problem_name, results_dict):
    x_i     = results_dict["cell_centers"]
    mu      = results_dict["mu"]
    psi_avg = results_dict["psi_avg"]

    out_file = results_dir / f"{problem_name}_cell_average_angular_flux.txt"
    with open(out_file, "w") as f:
        f.write(f"CELL-AVERAGE ANGULAR FLUX psi_{{i,m}} FOR {problem_name}\n")
        f.write(f"{'Angle m':<10s}{'mu_m':<12s}")
        for x in x_i:
            f.write(f"{'x=' + f'{x:.2f}':<14s}")
        f.write("\n" + "-" * (22 + 14 * len(x_i)) + "\n")

        for m in range(len(mu)):
            f.write(f"{m:<10d}{mu[m]:<+12.6f}")
            for i in range(len(x_i)):
                f.write(f"{psi_avg[m, i]:<14.6e}")
            f.write("\n")


def save_convergence_summary(results_dir, problem_name, results_dict):
    l_inf_hist = results_dict["l_inf_history"]
    rho_hist   = results_dict["rho_history"]
    n_iter     = results_dict["n_iterations"]
    C_L        = results_dict["C_L"]
    C_R        = results_dict["C_R"]

    out_file = results_dir / f"{problem_name}_convergence_and_summary.txt"
    with open(out_file, "w") as f:
        f.write(f"CONVERGENCE & BOUNDARY SUMMARY FOR {problem_name}\n")
        f.write("=" * 55 + "\n")
        f.write(f"Total Iterations (s):      {n_iter}\n")
        f.write(f"Left Boundary Factor C_L:  {C_L:.8f}\n")
        f.write(f"Right Boundary Factor C_R: {C_R:.8f}\n")
        f.write("=" * 55 + "\n\n")
        f.write(f"{'Iter s':<10s}{'||phi^(s) - phi^(s-1)||_inf':<28s}{'Est. Spectral Radius rho^(s)':<28s}\n")
        for idx in range(n_iter):
            f.write(f"{idx + 1:<10d}{l_inf_hist[idx]:<28.6e}{rho_hist[idx]:<28.6f}\n")

