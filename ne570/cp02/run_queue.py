from pathlib import Path
import subprocess
import sys

project_root = Path.cwd()


def find_case_scripts(root_dir):
    script_paths = []

    for path in root_dir.rglob("run_me.py"):
        if path.parent == root_dir:
            continue

        if path.is_file():
            script_paths.append(path)

    return sorted(script_paths)


def execute_queue():
    scripts = find_case_scripts(project_root)
    if not scripts:
        print("XXX No `run_me.py` files found. XXX")
        return

    print(f"Found {len(scripts)} case simulations in the queue.")

    for script in scripts:
        sim_dir = script.parent
        case_name = sim_dir.name
        print(f"\n=========================================")
        print(f" STARTING CASE: {case_name}")
        print(f"=========================================")

        cmd = [sys.executable, "run_me.py"]

        try:
            subprocess.run(cmd, cwd=sim_dir, check=True)
            print(f"\n!!! Finished Case: {case_name} successfully. !!!")
        except subprocess.CalledProcessError:
            print(f"\nXXX Simulation crashed in case `{case_name}` XXX")
        except KeyboardInterrupt:
            print("\nExiting safely.")
            break


if __name__ == "__main__":
    execute_queue()
