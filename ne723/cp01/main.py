# choose which problem by uncommenting the desired import
import test_a as problem
#import test_b as problem
#import test_c as problem

# !!! you can switch the mode of this file to run the plotting feature !!!

# CALCULATION MODE (calc)
#  - calc mode is the default mode
#  - running `python main.py` executes uncommented test (imported as problem)

# PLOTTING MODE (plot)
#  - you can run plot mode by passing a command line argument
#  - `python main.py --mode plot` will execute the script in the plotting mode
#  - plot mode is decoupled from the solution mode so you dont have to run an
#      hour long test (test b) to manipulate the results

# package imports
import numpy as np
from pathlib import Path
import time
import sys

# module imports
import mesh_builder
import output_manager
import plots
import quadrature
import solvers


# running the problem
def main():
    start_time = time.time()

    # execution mode settings
    mode = "calc"  # do calculation by default
    #mode = "plot"  # do plotting by default

    # check the command line arguments
    if len(sys.argv) > 1:
        for idx, arg in enumerate(sys.argv):
            # continue if there is a mode passed after `--mode`
            if arg == "--mode" and idx + 1 < len(sys.argv):
                mode = sys.argv[idx + 1].lower()

    # setting up results
    results_dir = Path(f"results-{problem.problem_name}")

    # safety checks for if the directory exists or not based on mode
    if mode == "plot":
        print(f"\n!!! [PLOTTING MODE] EXECUTING !!!")
        if results_dir.exists():
            print(f"\n    Generating plots in existing results dir '{results_dir}'")
            plots.generate_report_plots(results_dir)
            print(f"\n [PLOTTING MODE] COMPLETE !!!")
            return

        if not results_dir.exists():
            msg = f"Directory '{results_dir}' does not exist! Run the current case in `calc` mode before executing `plot` mode."
            raise FileNotFoundError(msg)

    if mode == "calc":
        if results_dir.exists():
            print(f"!!! WARNING !!! The directory '{results_dir}' already exists!")
            print("You will not be able to run in 'calc' mode until this directorry has a different name.")
            print("Either rename or delete the directory")
            return

    if mode not in ["calc", "plot"]:
        msg = f"Unsupported mode '{mode}'. Supported modes = ['calc', 'plot']"
        raise ValueError(msg)

    results_dir.mkdir(parents=True, exist_ok=True)

    log_filename = results_dir / f"{problem.problem_name}.log"

    print(f"\n [CALCULATION MODE] BEGINNING")
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

    print(f"\n [CALCULATION MODE] COMPLETE")
    return


if __name__ == "__main__":
    main()
