from pathlib import Path
import subprocess

input_file = "input.in"
mcnp_env_path = Path("/home/jfs/.config/mcnp/mcnp631.bash")
cores = "14"


def get_sourced_env(script_path):
    cmd = f"source {script_path} && env"
    output = subprocess.check_output(cmd, shell=True, executable="/bin/bash")

    env_dict = {}
    for line in output.decode("utf-8").splitlines():
        if "=" in line:
            key, value = line.split("=", 1)
            env_dict[key] = value
    return env_dict


def run_simulation():
    current_dir = Path.cwd()

    if not (current_dir / input_file).exists():
        print(f"[Error] Input file '{input_file}' not found in {current_dir}")
        return

    run_env = get_sourced_env(mcnp_env_path)
    run_env["OMP_NUM_THREADS"] = cores

    cmd = ["nice", "-n", "19", "mcnp6", f"i={input_file}", "tasks", cores]
    subprocess.run(cmd, env=run_env, check=True)


if __name__ == "__main__":
    run_simulation()
