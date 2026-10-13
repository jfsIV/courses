# choose which problem by uncommenting the desired import
#import test_a as problem
import test_b as problem
#import test_c as problem

# package imports
import numpy as np
from pathlib import Path
import time

# module imports
import output_manager
import mesh_builder
import quadrature
import solvers


# running the problem
def main():
    start_time = time.time()

    # setting up results
    results_dir = Path(f"results-{problem.problem_name}")
    results_dir.mkdir(parents=True, exist_ok=True)

    log_filename = results_dir / f"{problem.problem_name}.log"

    with open(log_filename, "w") as log_file:
        output_manager.print_problem_setup(problem, log_file)

        # discretize space and angle
        mesh_dict = mesh_builder.discretize_space(problem)
        quad_dict = quadrature.discretize_angle(problem)

        output_manager.log_time(
            start_time, f"Problem Setup Complete", file_obj=log_file
        )

        # fixed source problem
        if problem.problem_type == "fixed_source":
            msg = "\n  Beginning Inner Source Iteration Solver"
            output_manager.log_msg(msg, log_file)

            results_dict = solvers.fixed_source_solver(
                mesh_dict, quad_dict, problem, k_eff=1.0, file_obj=log_file
            )

            msg = f"Fixed source iterations complete"
            output_manager.log_time(start_time, msg, log_file)


        # k-eigenvalue criticality search problem
        if problem.problem_type == "k-eigenvalue":
            msg = "\n  Beginning Outer Criticality Search Solver"
            output_manager.log_msg(msg, log_file)

            results_dict = solvers.critical_width_solver(
                quad_dict, problem, file_obj=log_file
            )

            msg = f"Criticality search complete"
            output_manager.log_time(start_time, msg, log_file)


        # catching other cases
        if problem.problem_type not in ["fixed_source", "k-eigenvalue"]:
            msg = f"Unsupported problem type: {problem.problem_type}"
            raise ValueError(msg)

        # output results
        msg = "\n  Exporting results"
        output_manager.log_msg(msg, log_file)

        output_manager.save_results(
            results_dir, problem, mesh_dict, results_dict
        )


if __name__ == "__main__":
    main()
