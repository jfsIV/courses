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
    return
