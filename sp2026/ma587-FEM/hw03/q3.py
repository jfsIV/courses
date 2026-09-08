import numpy as np
import matplotlib.pyplot as plt
from mpi4py import MPI
from dolfinx import mesh, fem
from dolfinx.fem.petsc import LinearProblem
import ufl
from ufl import dx, grad, inner, ds, cos, pi

def solve_robin(gamma_val, n=32):
    # 1. Mesh and Function Space
    domain = mesh.create_unit_square(MPI.COMM_WORLD, n, n)
    V = fem.functionspace(domain, ("Lagrange", 1))

    # 2. Define Exact Solution and Source Terms
    x = ufl.SpatialCoordinate(domain)
    u_exact_ufl = cos(pi*x[0]) * cos(pi*x[1])
    f = 2 * (pi**2) * u_exact_ufl
    
    # Boundary Normal and g function
    normal = ufl.FacetNormal(domain)
    gamma = fem.Constant(domain, float(gamma_val))
    # g = gamma*u + du/dn
    g = gamma * u_exact_ufl + ufl.dot(grad(u_exact_ufl), normal)

    # 3. Variational Form
    u = ufl.TrialFunction(V)
    v = ufl.TestFunction(V)
    a = inner(grad(u), grad(v)) * dx + gamma * u * v * ds
    L = f * v * dx + g * v * ds

    # 4. Solve
    # Using a unique prefix for each gamma to avoid PETSc option conflicts
    problem = LinearProblem(a, L, petsc_options={"ksp_type": "preonly", "pc_type": "lu"},
                            petsc_options_prefix=f"g{str(gamma_val).replace('.','_').replace('-','m')}_")
    uh = problem.solve()

    # 5. Error Computation
    # L2 Error
    error_L2_form = fem.form(inner(uh - u_exact_ufl, uh - u_exact_ufl) * dx)
    l2_err = np.sqrt(domain.comm.allreduce(fem.assemble_scalar(error_L2_form), MPI.SUM))
    
    # Max (Infinity) Error
    u_ex_func = fem.Function(V)
    u_ex_func.interpolate(lambda x: np.cos(np.pi*x[0]) * np.cos(np.pi*x[1]))
    max_err = np.max(np.abs(uh.x.array - u_ex_func.x.array))

    return l2_err, max_err

# Values to test
gammas = [1e-14, -1e-14, 0.1, -0.1, 1.0, -1.0, 2.0, -2.0, 10.0, -10.0]
results = []

for g in gammas:
    l2, mmax = solve_robin(g)
    results.append((g, l2, mmax))

# --- Print LaTeX Table ---
print("\n% LaTeX Table Output:")
print("\\begin{table}[h]")
print("\\centering")
print("\\begin{tabular}{|l|l|l|}")
print("\\hline")
print("$\\gamma$ & $L_2$ Error & Max Error \\\\ \\hline")
for res in results:
    print(f"{res[0]:.1e} & {res[1]:.4e} & {res[2]:.4e} \\\\")
print("\\hline")
print("\\end{tabular}")
print("\\caption{Errors for varying $\\gamma$}")
print("\\end{table}\n")

# --- Plotting ---
plt.figure(figsize=(10, 6))
g_vals = [r[0] for r in results]
l2_vals = [r[1] for r in results]
max_vals = [r[2] for r in results]

plt.scatter(g_vals, l2_vals, label='$L_2$ Error', color='blue')
plt.scatter(g_vals, max_vals, label='Max Error', color='red', marker='x')
plt.yscale('log')
plt.xlabel('$\\gamma$')
plt.ylabel('Error Magnitude')
plt.title('Error Sensitivity to Robin Parameter $\\gamma$')
plt.legend()
plt.grid(True, which="both", ls="-", alpha=0.2)
plt.savefig("q3.png", dpi=600)
plt.close()
