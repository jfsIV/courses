import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path


def generate_report_plots(results_dir):
    results_dir = Path(results_dir)
    plots_dir = results_dir / "plots"
    plots_dir.mkdir(parents=True, exist_ok=True)

    npz_path = next(results_dir.glob("*.npz"))
    data = np.load(npz_path)

    plot_angular_flux_2d(data, plots_dir)
    plot_cell_avg_scalar_flux(data, plots_dir)
    plot_cell_edge_scalar_flux(data, plots_dir)
    plot_cell_edge_current(data, plots_dir)
    plot_cell_avg_qd_factor(data, plots_dir)
    plot_cell_edge_qd_factor(data, plots_dir)
    plot_departure_from_diffusion_metric(data, plots_dir)
    plot_qd_gradient_metric(data, plots_dir)
    plot_convergence_history(data, plots_dir)
    plot_spectral_radius(data, plots_dir)


def plot_angular_flux_2d(data, plots_dir):
    x_i = data["cell_centers"]
    mu = data["mu"]
    psi_avg = data["psi_avg"]

    fig, ax = plt.subplots(figsize=(7.5, 4.8))
    mesh = ax.pcolormesh(x_i, mu, psi_avg, shading="nearest", cmap="viridis")
    fig.colorbar(mesh, ax=ax, label=r"$\psi_{i,m}$")

    ax.set_xlabel(r"$x_i$ (cm)")
    ax.set_ylabel(r"$\mu_m$")

    fig.tight_layout()
    fig.savefig(plots_dir / "01_angular_flux.png", dpi=600)
    plt.close(fig)


def plot_cell_avg_scalar_flux(data, plots_dir):
    fig, ax = plt.subplots(figsize=(7, 4.5))
    ax.plot(data["cell_centers"], data["phi_avg"], linewidth=1.6)
    ax.set_xlabel(r"$x_i$ (cm)")
    ax.set_ylabel(r"$\phi_i$")
    ax.grid(True, linestyle="--", alpha=0.6)
    fig.tight_layout()
    fig.savefig(plots_dir / "02_phi_avg.png", dpi=600)
    plt.close(fig)


def plot_cell_edge_scalar_flux(data, plots_dir):
    fig, ax = plt.subplots(figsize=(7, 4.5))
    ax.plot(data["cell_edges"], data["phi_edge"], linewidth=1.6)
    ax.set_xlabel(r"$x_{i+1/2}$ (cm)")
    ax.set_ylabel(r"$\phi_{i+1/2}$")
    ax.grid(True, linestyle="--", alpha=0.6)
    fig.tight_layout()
    fig.savefig(plots_dir / "03_phi_edge.png", dpi=600)
    plt.close(fig)


def plot_cell_edge_current(data, plots_dir):
    fig, ax = plt.subplots(figsize=(7, 4.5))
    ax.plot(data["cell_edges"], data["J_edge"], linewidth=1.6)
    ax.set_xlabel(r"$x_{i+1/2}$ (cm)")
    ax.set_ylabel(r"$J_{i+1/2}$")
    ax.grid(True, linestyle="--", alpha=0.6)
    fig.tight_layout()
    fig.savefig(plots_dir / "04_J_edge.png", dpi=600)
    plt.close(fig)


def plot_cell_avg_qd_factor(data, plots_dir):
    fig, ax = plt.subplots(figsize=(7, 4.5))
    ax.axhline(1.0 / 3.0, color="r", linestyle="--", linewidth=1.1, label=r"Diffusion Limit ($1/3$)")
    ax.plot(data["cell_centers"], data["E_avg"], linewidth=1.6, label=r"$E_i$")
    ax.set_xlabel(r"$x_i$ (cm)")
    ax.set_ylabel(r"$E_i$")
    ax.legend()
    ax.grid(True, linestyle="--", alpha=0.6)
    fig.tight_layout()
    fig.savefig(plots_dir / "05_E_avg.png", dpi=600)
    plt.close(fig)


def plot_cell_edge_qd_factor(data, plots_dir):
    fig, ax = plt.subplots(figsize=(7, 4.5))
    ax.axhline(1.0 / 3.0, color="r", linestyle="--", linewidth=1.1, label=r"Diffusion Limit ($1/3$)")
    ax.plot(data["cell_edges"], data["E_edge"], linewidth=1.6, label=r"$E_{i+1/2}$")
    ax.set_xlabel(r"$x_{i+1/2}$ (cm)")
    ax.set_ylabel(r"$E_{i+1/2}$")
    ax.legend()
    ax.grid(True, linestyle="--", alpha=0.6)
    fig.tight_layout()
    fig.savefig(plots_dir / "06_E_edge.png", dpi=600)
    plt.close(fig)


def plot_departure_from_diffusion_metric(data, plots_dir):
    fig, ax = plt.subplots(figsize=(7, 4.5))
    ax.plot(data["cell_centers"], data["M_E"], linewidth=1.6)
    ax.set_xlabel(r"$x_i$ (cm)")
    ax.set_ylabel(r"$M_i^E = |1/3 - E_i|$")
    ax.grid(True, linestyle="--", alpha=0.6)
    fig.tight_layout()
    fig.savefig(plots_dir / "07_metric_ME.png", dpi=600)
    plt.close(fig)


def plot_qd_gradient_metric(data, plots_dir):
    fig, ax = plt.subplots(figsize=(7, 4.5))
    ax.plot(data["cell_centers"], data["M_E_prime"], linewidth=1.6)
    ax.set_xlabel(r"$x_i$ (cm)")
    ax.set_ylabel(r"$M_i^{E'} = |(E_{i+1/2} - E_{i-1/2}) / \Delta x_i|$")
    ax.grid(True, linestyle="--", alpha=0.6)
    fig.tight_layout()
    fig.savefig(plots_dir / "08_metric_ME_prime.png", dpi=600)
    plt.close(fig)


def plot_convergence_history(data, plots_dir):
    l_inf_hist = data["l_inf_history"]
    iters = np.arange(1, len(l_inf_hist) + 1)

    fig, ax = plt.subplots(figsize=(7, 4.5))
    ax.semilogy(iters, l_inf_hist, linewidth=1.6)
    ax.set_xlabel(r"Iteration Index $s$")
    ax.set_ylabel(r"$\|\phi^{(s)} - \phi^{(s-1)}\|_\infty$")
    ax.grid(True, which="both", linestyle="--", alpha=0.6)
    fig.tight_layout()
    fig.savefig(plots_dir / "09_convergence_history.png", dpi=600)
    plt.close(fig)


def plot_spectral_radius(data, plots_dir):
    rho_hist = data["rho_history"][2:]
    iters = np.arange(3, len(rho_hist) + 3)

    fig, ax = plt.subplots(figsize=(7, 4.5))
    style = "o" if len(iters) == 1 else "-"
    ax.plot(iters, rho_hist, style, linewidth=1.6)
    ax.set_xlabel(r"Iteration Index $s$")
    ax.set_ylabel(r"$\rho^{(s)}$")
    ax.grid(True, linestyle="--", alpha=0.6)
    fig.tight_layout()
    fig.savefig(plots_dir / "10_spectral_radius.png", dpi=600)
    plt.close(fig)
