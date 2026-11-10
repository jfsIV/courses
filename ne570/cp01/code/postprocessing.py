import matplotlib.pyplot as plt
import numpy as np

# parameters
godiva_radius = 8.7313
surface_area = 4 * np.pi * (8.7313)**2

source_intensity = 5e12
power_correction = 100

# energy bins
energy_bins = np.array([
    0.0000E+00,
    1.2500E-01,
    3.7500E-01,
    6.2500E-01,
    8.7500E-01,
    1.1250E+00,
    1.3250E+00,
    1.5250E+00,
    1.7250E+00,
    1.9250E+00,
    2.1250E+00,
    2.3250E+00,
    2.5250E+00,
    2.7250E+00,
    3.0000E+00,
    3.4000E+00,
    3.8000E+00,
    4.2000E+00,
    4.6000E+00,
    5.0000E+00,
    5.4000E+00,
    5.8000E+00,
    6.2000E+00,
    6.6000E+00,
    7.0000E+00,
    7.4000E+00,
    7.8000E+00,
    8.2000E+00,
    8.6000E+00,
    9.0000E+00,
])

bin_centers = (energy_bins[2:] + energy_bins[1:-1]) / 2
bin_widths = energy_bins[1:] - energy_bins[:-1]


# results
mcnp_values = np.array([
    2.41584e-05, 1.12812e-04, 1.20815e-04, 9.97566e-05, 8.31784e-05, 5.73720e-05,
    4.99956e-05, 4.38075e-05, 3.84700e-05, 3.37853e-05, 2.99766e-05, 2.67044e-05,
    2.35965e-05, 2.78799e-05, 3.28460e-05, 2.54570e-05, 1.96693e-05, 1.49905e-05,
    1.14987e-05, 8.89181e-06, 6.84104e-06, 5.33017e-06, 4.11224e-06, 3.12831e-06,
    2.35719e-06, 1.76246e-06, 1.34639e-06, 1.00083e-06, 7.65143e-07
])

experimental_values = np.array([
    720, 718, 615, 498, 370, 320, 255, 212, 195, 155, 140, 125, 130,
    85, 75, 60, 45, 42, 40, 25, 20, 18, 17, 15, 12, 10, 8, 5,
])


# processing
bin_widths = energy_bins[1:] - energy_bins[:-1]
bin_widths = bin_widths[1:]

active_bins = energy_bins[1:]
mcnp_values = mcnp_values[1:]

# unit conversaion
mcnp_values /= bin_widths

mcnp_values *= source_intensity
mcnp_values /= power_correction
mcnp_values /= 1e5

angular_flux_to_current_factor = np.pi
mcnp_values *= angular_flux_to_current_factor


# output
plt.stairs(mcnp_values, active_bins, label="MCNP", color="blue")

plt.scatter(
    bin_centers, experimental_values,
    label="Experiment (Godiva)", color="red"
)

plt.xlabel("Energy  [MeV]", fontsize=11)
plt.ylabel(r"N(E) ($1.0\times10^{5}$ n/$\mathregular{cm^2\cdot MeV}$)", fontsize=11)

plt.xlim(0.125, 9.2)
plt.ylim(0.0, 850)

plt.grid(True, alpha=0.5)
plt.legend(fontsize=10, loc="upper right")

plt.tight_layout()
plt.savefig("spectrum_comparison.png")
plt.close()
