import numpy as np
import time
from mpi4py import MPI
from petsc4py import PETSc

from dolfinx import fem, mesh
from dolfinx.fem import petsc
import ufl

# -----------------------------
# Problem setup
# -----------------------------
T = 1.0
dt = 0.01
num_steps = int(T / dt)

M = 500

print("=== Building mesh ===")
domain = mesh.create_interval(MPI.COMM_WORLD, M, [0.0, 1.0])

try:
    V = fem.functionspace(domain, ("Lagrange", 1))
except:
    V = fem.FunctionSpace(domain, ("CG", 1))

coords = V.tabulate_dof_coordinates()[:, 0]

u = ufl.TrialFunction(V)
v = ufl.TestFunction(V)

# -----------------------------
# Boundary condition
# -----------------------------
def boundary(x):
    return np.isclose(x[0], 0.0) | np.isclose(x[0], 1.0)

bc_dofs = fem.locate_dofs_geometrical(V, boundary)
bc = fem.dirichletbc(PETSc.ScalarType(0), bc_dofs, V)

# -----------------------------
# Assemble matrices
# -----------------------------
print("=== Assembling matrices ===")

M_form = u * v * ufl.dx
K_form = ufl.inner(ufl.grad(u), ufl.grad(v)) * ufl.dx

M_mat = petsc.assemble_matrix(fem.form(M_form), bcs=[bc])
M_mat.assemble()

K_mat = petsc.assemble_matrix(fem.form(K_form), bcs=[bc])
K_mat.assemble()

def petsc_to_numpy(A):
    return A.convert("dense").getDenseArray()

M_np = petsc_to_numpy(M_mat)
K_np = petsc_to_numpy(K_mat)

A = M_np + 0.5 * dt * K_np
B = M_np - 0.5 * dt * K_np

A_inv = np.linalg.inv(A)

# -----------------------------
# Initial condition
# -----------------------------
def initial_condition(mu, sigma):
    return np.exp(-((coords - mu) ** 2) / (2 * sigma ** 2))

# -----------------------------
# FOM solver
# -----------------------------
def solve_FOM(mu, sigma):
    u_vec = initial_condition(mu, sigma)

    for n in range(num_steps):
        t = n * dt
        f_vec = np.sin(2 * np.pi * coords) * np.exp(-t)
        rhs = B @ u_vec + dt * f_vec
        u_vec = A_inv @ rhs

    return u_vec

# -----------------------------
# Snapshot generation
# -----------------------------
print("=== Generating snapshots ===")

param_samples = [(mu, sigma)
                 for mu in np.linspace(0.2, 0.8, 3)
                 for sigma in np.linspace(0.2, 0.8, 3)]

snapshots = []

for i, (mu, sigma) in enumerate(param_samples):
    print(f"[Snapshot {i+1}/9]")
    u_vec = initial_condition(mu, sigma)
    local_snaps = []

    for n in range(num_steps):
        t = n * dt
        f_vec = np.sin(2 * np.pi * coords) * np.exp(-t)
        rhs = B @ u_vec + dt * f_vec
        u_vec = A_inv @ rhs
        local_snaps.append(u_vec.copy())

    snapshots.append(np.array(local_snaps).T)

S = np.hstack(snapshots)
print("Snapshot matrix shape:", S.shape)

# -----------------------------
# POD
# -----------------------------
print("=== Computing POD ===")
t0 = time.time()

U, Sigma, VT = np.linalg.svd(S, full_matrices=False)

print("SVD time:", time.time() - t0)

r = 15
Phi = U[:, :r]

# -----------------------------
# ROM
# -----------------------------
print("=== Building ROM ===")

M_r = Phi.T @ M_np @ Phi
K_r = Phi.T @ K_np @ Phi

A_r = M_r + 0.5 * dt * K_r
B_r = M_r - 0.5 * dt * K_r

A_r_inv = np.linalg.inv(A_r)

def solve_ROM(mu, sigma):
    a = Phi.T @ initial_condition(mu, sigma)

    for n in range(num_steps):
        t = n * dt
        f_vec = np.sin(2 * np.pi * coords) * np.exp(-t)
        f_r = Phi.T @ f_vec
        rhs = B_r @ a + dt * f_r
        a = A_r_inv @ rhs

    return Phi @ a

# -----------------------------
# ROM on 100x100 grid
# -----------------------------
print("=== Evaluating ROM on 100x100 grid ===")

mus = np.linspace(0, 1, 100)
sigmas = np.linspace(0.1, 0.9, 100)

rom_start = time.time()

for i, mu in enumerate(mus):
    if i % 10 == 0:
        print(f"Row {i}/100")
    for sigma in sigmas:
        solve_ROM(mu, sigma)

rom_total_time = time.time() - rom_start

# -----------------------------
# FOM vs ROM timing (validation set)
# -----------------------------
print("=== Runtime comparison & error analysis ===")

# Build parameter grid
mus = np.linspace(0, 1, 100)
sigmas = np.linspace(0.1, 0.9, 100)
param_grid = [(mu, sigma) for mu in mus for sigma in sigmas]

# Random selection (reproducible)
np.random.seed(42)
indices = np.random.choice(len(param_grid), 9, replace=False)
test_points = [param_grid[i] for i in indices]

fom_times = []
rom_times = []
errors = []

for i, (mu, sigma) in enumerate(test_points):
    print(f"[Test {i+1}/9] mu={mu:.3f}, sigma={sigma:.3f}")

    # FOM
    t0 = time.time()
    u_fom = solve_FOM(mu, sigma)
    fom_times.append(time.time() - t0)

    # ROM
    t0 = time.time()
    u_rom = solve_ROM(mu, sigma)
    rom_times.append(time.time() - t0)

    err = np.linalg.norm(u_fom - u_rom) / np.linalg.norm(u_fom)
    errors.append(err)

avg_error = np.mean(errors)

# -----------------------------
# LaTeX Table Output
# -----------------------------
print("\n=== LaTeX TABLE (copy/paste) ===\n")

print("\\begin{tabular}{c c c}")
print("\\hline")
print("$\\mu$ & $\\sigma$ & Relative $L^2$ Error \\\\")
print("\\hline")

for (mu, sigma), err in zip(test_points, errors):
    print(f"{mu:.4f} & {sigma:.4f} & {err:.6e} \\\\")

print("\\hline")
print("\\end{tabular}")

# -----------------------------
# Compact LaTeX list of errors
# -----------------------------
print("\n=== LaTeX ERROR LIST ===\n")

error_list = ", ".join([f"{e:.3e}" for e in errors])
print(error_list)

# -----------------------------
# Average error
# -----------------------------
print("\n=== Average Error ===")
print(f"{avg_error:.6e}")

# -----------------------------
# Timing summary
# -----------------------------
avg_fom = np.mean(fom_times)
avg_rom = np.mean(rom_times)

print("\n=== TIMING SUMMARY ===")
print(f"{'Model':<10} | {'Avg Time (s)':<15} | {'Total Time (s)':<15}")
print("-" * 45)
print(f"{'FOM':<10} | {avg_fom:<15.6f} | {sum(fom_times):<15.6f}")
print(f"{'ROM':<10} | {avg_rom:<15.6f} | {sum(rom_times):<15.6f}")

print("\nSpeed-up (per solve): {:.2f}x".format(avg_fom / avg_rom))
