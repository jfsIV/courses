# choose problem by uncommenting the desired import
from input_files import test_a as problem
#from input_files import test_b as problem
#from input_files import test_c as problem
#from input_files import test_d1 as problem
#from input_files import test_d2 as problem


# !!! you can switch the mode of this file to run the plotting feature !!!
execution_mode = "calc"
#execution_mode = "plot"

# CALCULATION MODE (calc)
#  - calc mode is the default mode
#  - running `python main.py` executes uncommented test (imported as problem)

# PLOTTING MODE (plot)
#  - you can run plot mode by passing a command line argument
#  - `python main.py --mode plot` will execute the script in the plotting mode


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


def main():
    start_time = time.time()

    # setting up results path
    base_results_dir = Path("results")
    results_dir = base_results_dir / f"{problem.problem_name}"

    # initial mode checks
    if execution_mode not in ["calc", "plot"]:
        msg = f"Unsupported execution mode `{exeuction_mode}`. Use mode = ['calc', 'plot']"
        raise ValueError(msg)

    if execution_mode == "plot":
        print(f"\n!!! Executing in `PLOT` !!!")

        if not results_dir.exists():
            msg = f"Directory '{results_dir}' does not exist! Run the current case in `calc` mode before executing `plot` mode."
            raise FileNotFoundError(msg)

        print(f"\n    Overwriting plots in existing results dir '{results_dir}'")
        plots.generate_report_plots(results_dir)
        print(f"\n `PLOT` Execution of `{problem.problem_name}` Complete !!!")
        return

    if execution_mode == "calc":
        if results_dir.exists():
            print(f"XXX WARNING XXX The directory '{results_dir}' already exists!")
            print("You will not be able to run in 'calc' mode until this directory has a different name.")
            print("Either rename or delete the directory")
            return

    results_dir.mkdir(parents=True, exist_ok=True)
    log_name = results_dir / f"{problem.problem_name}.log"

    # executing calculation mode
    print("\n!!! Executing in `CALC` !!!")
    with open(log_name, "w") as log_file:
        output_manager.print_problem_setup(problem, log_file)

        mesh_dict = mesh_builder.discretize_space(problem)
        quad_dict = quadrature.discretize_angle(problem)

        msg = "Problem setup complete"
        output_manager.log_time(start_time, msg, log_file)

        msg = "\nBeginning to Solve the Fixed-Source Transport Problem using QD"
        output_manager.log_msg(msg, log_file)

        results_dict = solvers.qd_solver(
            problem, mesh_dict, quad_dict, log_file
        )

        msg = "Completed Solving the Fixed-Source Transport Problem using QD"
        output_manager.log_time(start_time, msg, log_file)

        msg = "\nExporting Results"
        output_manager.log_msg(msg, log_file)

        output_manager.save_results(problem, results_dict, results_dir)

    print(f"\n!!! `CALC` Execution of `{problem.problem_name}` Complete !!!")
    return


if __name__ == "__main__":
    main()
