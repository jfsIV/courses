from pathlib import Path
import shutil

"""
# part 1

## constants
- 1e7 photons/s source
- 30 cm from source to detector

## simulations
Energies:
    - 0.5, 1, 2, 3, 4, 6, 8, 10
Thicknesses:
    - 1, 2, 4, 7, 10

- try to get 5% uncertainty
- photon production tables in MCNP
- tally is rad/hr/src photon, so multiply by source strength

## things to change
1. {OPTICAL_THICKNESS} mu * r or the optical thickness of the shield
2. {SOURCE_ENERGY} starting source energy
3. {SCATTERING_ENERGY} cut-off energy for the tally (just below source energy)
"""

# input parameters
source_energies = [0.5, 1, 2, 3, 4, 6, 8, 10]  # MeV
optical_thicknesses = [1, 2, 4, 7, 10]

lead_mu_dict = {  # taken from problem statements slides
    0.5: 1.746,
    1.0: 0.782,
    2.0: 0.510,
    3.0: 0.472,
    4.0: 0.469,
    6.0: 0.492,
    8.0: 0.521,
    10.0: 0.549,
}


# functions
def update_mcnp_input(
    case_path, replacements,
    template_name="input-template.in"
):
    input_file = case_path / template_name

    content = input_file.read_text()
    for placeholder, value in replacements.items():
        content = content.replace(placeholder, value)

    input_file.write_text(content)
    input_file.rename(case_path / "input.in")


def update_run_script(
    case_path, replacements,
    template_name="run-template.py"
):
    run_script_file = case_path / template_name

    if not run_script_file.exists():
        msg = f"  Script template `{template_name}` not found. Continuing..."
        print(msg)
        return

    content = run_script_file.read_text()
    for placeholder, value in replacements.items():
        content = content.replace(placeholder, value)

    run_script_file.write_text(content)
    run_script_file.rename(case_path / "run_me.py")


def create_case(
    source_energy,
    optical_thickness,
    template_dir="template",
    cases_dir="cases"
):
    template_path = Path(template_dir)
    cases_path = Path(cases_dir)

    case_name = f"E{source_energy:04.1f}MeV-{optical_thickness:02d}mfp"
    output_path = cases_path / case_name

    if output_path.exists():
        msg = f"Directory `{output_path}` already exists. Overwrite? [y]: "
        """
        response = input(msg)
        response = response.strip().lower()

        if response not in ["y", "yes"]:
            print(f"XXX Skipping case `{case_name}` XXX")
            return
        """

        shutil.rmtree(output_path)

    shutil.copytree(template_path, output_path)

    mu = lead_mu_dict[source_energy]
    shield_thickness_cm = optical_thickness / mu
    scattering_energy = energy - 0.001

    input_replacements = {
        "SOURCE_ENERGY": f"{energy:.1f}",
        "SHIELD_THICKNESS": f"{shield_thickness_cm:.4f}",
        "SCATTERING_ENERGY": f"{scattering_energy:.3f}"
    }

    run_replacements = {
    }

    update_mcnp_input(output_path, input_replacements)
    update_run_script(output_path, run_replacements)

    print(f"!!! Successfully created case `{case_name}` !!!")


# running
if __name__ == "__main__":
    for energy in source_energies:
        for thickness in optical_thicknesses:
            create_case(energy, thickness)
