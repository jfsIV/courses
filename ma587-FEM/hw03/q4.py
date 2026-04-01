import numpy as np
import matplotlib.pyplot as plt
from mpi4py import MPI
from dolfinx import mesh, fem
from dolfinx.fem.petsc import LinearProblem
import ufl
import basix.ufl
from ufl import dx, grad, inner, sin, pi, div

def solve_biharmonic_mixed(n_elements):
    comm = MPI.COMM_WORLD
    # 1. Mesh
    domain = mesh.create_unit_square(comm, n_elements, n_elements)
    
    # 2. Function Space (Using P2-P2 Lagrange)
    # This is the Ciarlet-Raviart mixed formulation
    P2 = basix.ufl.element("Lagrange", domain.topology.cell_name(), 2)
    mel = basix.ufl.mixed_element([P2, P2])
    V = fem.functionspace(domain, mel)

    # 3. Source Term f = Delta^2(u_exact)
    # u_exact = sin^2(pi*x) * sin^2(pi*y)
    x = ufl.SpatialCoordinate(domain)
    u_exact_ufl = (sin(pi*x[0])**2) * (sin(pi*x[1])**2)
    
    # Define f as Delta(Delta(u))
    f = div(grad(div(grad(u_exact_ufl))))

    # 4. Boundary Conditions (u = 0 on Gamma)
    tdim = domain.topology.dim
    fdim = tdim - 1
    domain.topology.create_connectivity(fdim, tdim)
    facets = mesh.exterior_facet_indices(domain.topology)
    
    # Enforce u = 0 on the first component of the mixed space
    V0, _ = V.sub(0).collapse()
    u_zero = fem.Function(V0)
    u_zero.interpolate(lambda x: np.zeros_like(x[0]))
    
    dofs = fem.locate_dofs_topological((V.sub(0), V0), fdim, facets)
    bc = fem.dirichletbc(u_zero, dofs, V.sub(0))

    # 5. Variational Form
    (u, w) = ufl.TrialFunctions(V)
    (v, q) = ufl.TestFunctions(V)
    
    # Form: (grad u, grad q) - (w, q) + (grad w, grad v) = (f, v)
    a = inner(grad(u), grad(q)) * dx - inner(w, q) * dx + inner(grad(w), grad(v)) * dx
    L = inner(f, v) * dx

    # 6. Solve with prefix and direct LU solver
    # petsc_options_prefix is REQUIRED in your FEniCSx version
    problem = LinearProblem(
        a, L, bcs=[bc], 
        petsc_options={
            "ksp_type": "preonly", 
            "pc_type": "lu", 
            "pc_factor_mat_solver_type": "mumps"
        },
        petsc_options_prefix=f"bh_n{n_elements}_"
    )
    
    sol = problem.solve()
    uh = sol.sub(0).collapse()

    # 7. Error Computation
    # L2 Error
    error_form = fem.form(inner(uh - u_exact_ufl, uh - u_exact_ufl) * dx)
    l2_local = fem.assemble_scalar(error_form)
    l2_err = np.sqrt(comm.allreduce(l2_local, MPI.SUM))
    
    # Max error
    u_ex_func = fem.Function(V0)
    u_ex_func.interpolate(lambda x: np.sin(np.pi*x[0])**2 * np.sin(np.pi*x[1])**2)
    max_local = np.max(np.abs(uh.x.array - u_ex_func.x.array)) if len(uh.x.array) > 0 else 0
    max_err = comm.allreduce(max_local, MPI.MAX)

    return l2_err, max_err

# --- Study Execution ---
ns = [8, 16, 32, 64]
results = []
rank = MPI.COMM_WORLD.rank

for n in ns:
    l2, mmax = solve_biharmonic_mixed(n)
    if rank == 0:
        results.append((n, l2, mmax))
        print(f"Computed N={n:2d} | L2 Error: {l2:.4e}")

# --- LaTeX Table and Plotting (Rank 0 only) ---
if rank == 0:
    print("\n% LaTeX Table Output:")
    print("\\begin{table}[h]\n\\centering\n\\begin{tabular}{|c|c|c|}\n\\hline")
    print("$N$ & $L_2$ Error & Max Error \\\\ \\hline")
    for n, l2, mmax in results:
        print(f"{n} & {l2:.4e} & {mmax:.4e} \\\\")
    print("\\hline\n\\end{tabular}\n\\caption{Mixed Biharmonic Convergence}\n\\end{table}\n")

    # Plotting
    n_vals = [r[0] for r in results]
    l2_vals = [r[1] for r in results]
    plt.figure(figsize=(8, 6))
    plt.loglog(n_vals, l2_vals, 'bo-', label='Mixed FEM $L_2$ Error')
    
    # Theoretical O(h^2) line
    h = 1.0 / np.array(n_vals)
    plt.loglog(n_vals, (h**2) * (l2_vals[0]/h[0]**2), 'k--', alpha=0.5, label='$O(h^2)$ reference')
    
    plt.xlabel('N (Elements per side)')
    plt.ylabel('Error')
    plt.title('Biharmonic Convergence: Mixed Formulation')
    plt.legend()
    plt.grid(True, which="both", ls="-", alpha=0.2)
    plt.savefig('q4.png', dpi=600)
    plt.close()
