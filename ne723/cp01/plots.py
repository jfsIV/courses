import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path


def generate_report_plots(results_dir):
    problem_name = results_dir.name.replace("results-", "")
    history_file = results_dir / f"{problem_name}_convergence_history.txt"

    plot_dir = results_dir / "plots"
    plot_dir.mkdir(parents=True, exist_ok=True)
    print(f"\nSavings plots to '{plot_dir}'")

    with open(history_file, "r") as f:
        header = f.readline()

    plot_common_results(results_dir, plot_dir, problem_name)

    if "INNER ITERATION" in header:
        plot_fixed_source_convergence(results_dir, plot_dir, problem_name, history_file)
    if "CRITICALITY SEARCH" in header:
        plot_critical_width_history(results_dir, plot_dir, problem_name, history_file)

    return


def plot_common_results(results_dir, plot_dir, problem_name):
    moments_file = results_dir / f"{problem_name}_cell_moments_and_residuals.txt"
    edges_file = results_dir / f"{problem_name}_cell_edge_flux_and_current.txt"
    angular_file = results_dir / f"{problem_name}_angular_flux_matrix.txt"

    if moments_file.exists():
        data = np.loadtxt(moments_file, skiprows=2)
        x_centers = data[:, 1]

        plot_cell_average_flux(results_dir, plot_dir, problem_name, x_centers, data[:, 2])
        plot_spatial_moment_flux(results_dir, plot_dir, problem_name, x_centers, data[:, 3])
        plot_residual_dr0(results_dir, plot_dir, problem_name, x_centers, data[:, 4])
        plot_residual_dr1(results_dir, plot_dir, problem_name, x_centers, data[:, 5])
        plot_residual_dR0(results_dir, plot_dir, problem_name, x_centers, data[:, 6])
        plot_residual_dR1(results_dir, plot_dir, problem_name, x_centers, data[:, 7])

    if edges_file.exists():
        data = np.loadtxt(edges_file, skiprows=2)
        x_edges = data[:, 1]
        plot_cell_edge_flux(results_dir, plot_dir, problem_name, x_edges, data[:, 2])
        plot_cell_edge_current(results_dir, plot_dir, problem_name, x_edges, data[:, 3])

    if angular_file.exists():
        plot_angular_flux_2d(results_dir, plot_dir, problem_name, angular_file)
        plot_integrated_angular_flux(results_dir, plot_dir, problem_name, angular_file)


def plot_cell_average_flux(results_dir, plot_dir, problem_name, x_centers, phi_bar):
    plt.figure(figsize=(7, 4.5))
    plt.plot(x_centers, phi_bar)
    plt.xlabel("Position x [cm]")
    plt.ylabel("Cell-Average Flux, phi_bar")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_cell_average_flux.png", dpi=600)
    plt.close()


def plot_spatial_moment_flux(results_dir, plot_dir, problem_name, x_centers, phi_hat):
    plt.figure(figsize=(7, 4.5))
    plt.plot(x_centers, phi_hat)
    plt.xlabel("Position x [cm]")
    plt.ylabel("First Spatial Moment, phi_hat")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_spatial_moment_flux.png", dpi=600)
    plt.close()


def plot_residual_dr0(results_dir, plot_dir, problem_name, x_centers, dr_0):
    plt.figure(figsize=(7, 4.5))
    plt.plot(x_centers, dr_0)
    plt.xlabel("Position x [cm]")
    plt.ylabel("Scheme Balance Residual, dr_0")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_residual_dr0.png", dpi=600)
    plt.close()


def plot_residual_dr1(results_dir, plot_dir, problem_name, x_centers, dr_1):
    plt.figure(figsize=(7, 4.5))
    plt.plot(x_centers, dr_1)
    plt.xlabel("Position x [cm]")
    plt.ylabel("Scheme First Moment Residual, dr_1")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_residual_dr1.png", dpi=600)
    plt.close()


def plot_residual_dR0(results_dir, plot_dir, problem_name, x_centers, dR_0):
    plt.figure(figsize=(7, 4.5))
    plt.plot(x_centers, dR_0)
    plt.xlabel("Position x [cm]")
    plt.ylabel("Iterative Balance Residual, dR_0")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_residual_dR0.png", dpi=600)
    plt.close()


def plot_residual_dR1(results_dir, plot_dir, problem_name, x_centers, dR_1):
    plt.figure(figsize=(7, 4.5))
    plt.plot(x_centers, dR_1)
    plt.xlabel("Position x [cm]")
    plt.ylabel("Iterative First Moment Residual, dR_1")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_residual_dR1.png", dpi=600)
    plt.close()


def plot_cell_edge_flux(results_dir, plot_dir, problem_name, x_edges, phi_edges):
    plt.figure(figsize=(7, 4.5))
    plt.plot(x_edges, phi_edges)
    plt.xlabel("Interface Position, x [cm]")
    plt.ylabel("Cell-Edge Flux, phi_edge")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_cell_edge_flux.png", dpi=600)
    plt.close()


def plot_cell_edge_current(results_dir, plot_dir, problem_name, x_edges, j_edges):
    plt.figure(figsize=(7, 4.5))
    plt.plot(x_edges, j_edges)
    plt.xlabel("Interface Position, x [cm]")
    plt.ylabel("Cell-Edge Current, j_edge")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_cell_edge_current.png", dpi=600)
    plt.close()


def plot_angular_flux_2d(results_dir, plot_dir, problem_name, angular_file_path):
    with open(angular_file_path, 'r') as f:
        lines = f.readlines()

    pos_line = lines[2].strip()
    cell_segments = [x.strip() for x in pos_line.split('|')[1:] if x.strip() and '=' in x]
    cell_positions = [float(x.split('=')[1].strip()) for x in cell_segments]

    matrix_data = []
    for line in lines[4:]:
        if "Angle" in line:
            parts = line.split()
            matrix_data.append([float(val) for val in parts[2:]])

    psi_matrix = np.array(matrix_data)
    n_angles = psi_matrix.shape[0]
    mu, _ = np.polynomial.legendre.leggauss(n_angles)

    plt.figure(figsize=(8, 4.5))
    X, Y = np.meshgrid(cell_positions, mu)
    plt.pcolormesh(X, Y, psi_matrix, cmap='viridis', shading='nearest')
    plt.colorbar().set_label("Angular Flux, psi")
    plt.xlabel("Position, x [cm]")
    plt.ylabel("Angle, mu")
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_angular_flux_2d.png", dpi=600)
    plt.close()


def plot_integrated_angular_flux(results_dir, plot_dir, problem_name, angular_file_path):
    with open(angular_file_path, 'r') as f:
        lines = f.readlines()

    pos_line = lines[2].strip()
    cell_segments = [x.strip() for x in pos_line.split('|')[1:] if x.strip() and '=' in x]
    cell_positions = [float(x.split('=')[1].strip()) for x in cell_segments]

    matrix_data = []
    for line in lines[4:]:
        if "Angle" in line:
            parts = line.split()
            matrix_data.append([float(val) for val in parts[2:]])

    psi_matrix = np.array(matrix_data)
    integrated_psi = np.sum(psi_matrix, axis=0)

    plt.figure(figsize=(7, 4.5))
    plt.plot(cell_positions, integrated_psi)
    plt.xlabel("Position, x [cm]")
    plt.ylabel("Total Angular Flux (sum over angles)")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_cumulative_angular_flux.png", dpi=600)
    plt.close()


def plot_fixed_source_convergence(results_dir, plot_dir, problem_name, history_file):
    raw_data = np.loadtxt(
        history_file,
        skiprows=3,
        converters={2: lambda s: float(s.replace('None', '0.0'))}
    )
    sampled_data = raw_data[::1000]

    if raw_data[-1, 0] != sampled_data[-1, 0]:
        sampled_data = np.vstack([sampled_data, raw_data[-1]])

    iters = sampled_data[:, 0]
    errors = sampled_data[:, 1]
    rhos = sampled_data[:, 2]

    plt.figure(figsize=(7, 4.5))
    plt.semilogy(iters, errors)
    plt.xlabel("Iteration index")
    plt.ylabel("L-infinity Error")
    plt.grid(True, which="both", linestyle='--', alpha=0.5)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_error_history.png", dpi=600)
    plt.close()

    plt.figure(figsize=(7, 4.5))
    plt.plot(iters, rhos)
    plt.xlabel("Iteration index")
    plt.ylabel("Spectral Radius")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_spectral_radius_history.png", dpi=600)
    plt.close()


def plot_critical_width_history(results_dir, plot_dir, problem_name, history_file):
    outer_lines = []
    with open(history_file, 'r') as f:
        capture = False
        for line in f:
            if "1. Outer loop" in line:
                capture = True
                continue
            if "2. Inner power" in line:
                break
            if capture and line.strip() and not line.startswith("Outer Step"):
                outer_lines.append(line)

    outer_iters = []
    outer_widths = []
    for line in outer_lines:
        parts = line.split()
        if len(parts) >= 2:
            try:
                outer_iters.append(int(parts[0]))
                outer_widths.append(float(parts[1]))
            except ValueError:
                continue

    plt.figure(figsize=(7, 4.5))
    plt.plot(outer_iters, outer_widths)
    plt.xlabel("Secant Search Step Index")
    plt.ylabel("Estimated Slab Width, a_crit [cm]")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig(plot_dir / f"{problem_name}_critical_width_history.png", dpi=600)
    plt.close()
