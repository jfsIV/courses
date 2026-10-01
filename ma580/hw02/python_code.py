import matplotlib.pyplot as plt
import numpy as np
import scipy.sparse as sp
import scipy.sparse.linalg as spla
import time


# Difference in generation the AI reported:
# - Slicing (2:end-1 vs 1:-1): Python uses 0-based indexing and exclusive upper bounds. The interior nodes 2:end-1 in MATLAB translate to 1:-1 in Python.
# - Meshgrid Orientation (indexing='ij'): Passing indexing='ij' to NumPy's meshgrid keeps coordinate matrix array structures aligned cleanly with standard row/column Cartesian indices.
# - Backslash Operator (\): MATLAB's G \ U(:,n) is handled via scipy.sparse.linalg.spsolve(G, U[:, n]) for sparse matrices.


def u0(x, y):
    """Initial condition function."""
    term1 = (((x - 0.125)**2 + (y - 0.125)**2) <= 0.01) * 10.0
    term2 = (((x - 0.5)**2 + (y - 0.5)**2) <= 0.01) * 5.0
    return term1 + term2


def get_discrete_laplacian(nx):
    """Computes discretization of Lu = -Laplacian u."""
    m = nx - 1
    I = sp.eye(m, format='csr')
    e = np.ones(m)

    # Create T and S blocks as sparse matrices
    T = sp.diags([-e, 4*e, -e], [-1, 0, 1], shape=(m, m), format='csr')
    S = sp.diags([-e, -e], [-1, 1], shape=(m, m), format='csr')

    # 2D Laplacian via Kronecker product
    A = sp.kron(I, T, format='csr') + sp.kron(S, I, format='csr')
    return A


def solve_heat_2D_implicit(mode):
    # I ADDED THIS AND MADE THIS FN OF MODE ---------------------------------- #
    """
    Parameters
    ----------
    mode : str
        - changes how problem is solved: ["default", "factored", "optimized"]
    """
    # END ADDITION ----------------------------------------------------------- #

    # PDE parameters
    kappa = 0.001

    # Spatial discretization
    nx = 2**7
    h = 1.0 / nx
    xi = np.linspace(0, 1, nx + 1)
    yi = xi
    Xi, Yi = np.meshgrid(xi, yi, indexing='ij')

    # The spatial discretization matrix 
    A = get_discrete_laplacian(nx)
    A = kappa * (1.0 / h**2) * A 

    # Evaluate initial state and extract interior nodes
    U0_full = u0(Xi, Yi)
    U0 = U0_full[1:-1, 1:-1]    

    # Time discretization
    t0 = 0
    Tf = 10
    nt = 500 + 1
    dt = Tf / (nt - 1)
    ti = np.linspace(t0, Tf, nt)

    # Time-stepping setup
    num_interior = (nx - 1)**2
    # U will store flattened solutions row by row across time steps
    U = np.zeros((num_interior, nt))
    U[:, 0] = U0.flatten() # Python defaults to row-major flattening
    I = sp.eye(num_interior, format='csr')

    print(f"Time integration in progress for {mode} approach...")
    start_time = time.time()

    G = I + dt * A
    # Convert to CSR format for efficient row-slicing and solving
    G = G.tocsr()

    # I ADDED THIS ----------------------------------------------------------- #
    # Changing Modes
    if mode == "factored":
        # python equivalent of basic `chol`
        G = G.tocsc()  # format splu is more efficient in
        G_lu_factors = spla.splu(G, permc_spec="NATURAL")
    if mode == "optimized":
        # python equivalent of `symamd`
        G = G.tocsc()  # format splu is more efficient in
        G_lu_factors = spla.splu(G, permc_spec="MMD_AT_PLUS_A")
    # END ADDITION ----------------------------------------------------------- #


    for n in range(nt - 1):
        # Show progress at integer time steps
        if ti[n] % 1 == 0:
            print(f"t = {ti[n]:4.2f}")

        # I ADDED THIS ------------------------------------------------------- #
        if mode == "default":
            # Solve the linear system (Baseline method: equivalent to backslash \)
            U[:, n + 1] = spla.spsolve(G, U[:, n])
        if mode in ["factored", "optimized"]:
            U[:, n + 1] = G_lu_factors.solve(U[:, n])
        # END ADDITION ------------------------------------------------------- #

    compute_time = time.time() - start_time
    print(f"Time integration for {mode} approach complete in {compute_time:.4f} seconds\n")

    # Store the solution back into a list of 2D grids (including boundary nodes)
    U_array = []
    for i in range(nt):
        U_mat = np.zeros((nx + 1, nx + 1))
        U_mat[1:-1, 1:-1] = U[:, i].reshape(nx - 1, nx - 1)
        U_array.append(U_mat)

    return U_array, ti, Xi, Yi


if __name__ == "__main__":
    # I ADDED THIS ----------------------------------------------------------- #
    for mode in ["default", "factored", "optimized"]:
        U_array, ti, Xi, Yi = solve_heat_2D_implicit(mode=mode)
        sampled_time = np.arange(0, 11)  # creating plots every second

        for sec in sampled_time:
            time_index = int(sec / (10/500))
            time_step = ti[time_index]

            plt.figure(figsize=(6,5))

            contour = plt.contourf(Xi, Yi, U_array[time_index], cmap="viridis", levels=50)
            plt.colorbar(contour, label="Temperature")

            plt.title(f"{mode.capitalize()} at time={time_step:.1f} sec")
            plt.xlabel("x")
            plt.ylabel("y")
            plt.tight_layout()

            file_name=f"hw02q4_{mode}_t{sec:02d}s.png"
            plt.savefig(file_name, dpi=600)
            plt.close()

        print(f"Finished plotting for {mode} approach")

    print(f"Finished all plotting")
    # END ADDITION ----------------------------------------------------------- #
