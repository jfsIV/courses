import numpy as np
import matplotlib.pyplot as plt
from mpi4py import MPI
from dolfinx import mesh, fem
from dolfinx.fem.petsc import LinearProblem
import ufl
from ufl import dx, grad, inner

def solve_poisson_problem(num_elements, degree):
    """
    Solves -u'' = 1 on (0, 1) with u(0) = u(1) = 0
    """
    # 1. Create mesh and function space
    domain = mesh.create_unit_interval(MPI.COMM_WORLD, num_elements)
    V = fem.functionspace(domain, ("Lagrange", degree))

    # 2. Define Boundary Conditions
    tdim = domain.topology.dim
    fdim = tdim - 1
    # Ensure connectivity is computed for boundary identification
    domain.topology.create_connectivity(fdim, tdim)
    boundary_facets = mesh.exterior_facet_indices(domain.topology)
    
    # Boundary value u = 0
    u_D = fem.Function(V)
    u_D.interpolate(lambda x: np.zeros_like(x[0]))
    bc = fem.dirichletbc(u_D, fem.locate_dofs_topological(V, fdim, boundary_facets))

    # 3. Define Variational Problem
    u = ufl.TrialFunction(V)
    v = ufl.TestFunction(V)
    f = fem.Constant(domain, 1.0)
    
    a = inner(grad(u), grad(v)) * dx
    L = f * v * dx

    # 4. Solve 
    # Added petsc_options_prefix to satisfy newer FEniCSx requirements
    problem = LinearProblem(
        a, L, bcs=[bc], 
        petsc_options={"ksp_type": "preonly", "pc_type": "lu"},
        petsc_options_prefix="poisson_" 
    )
    uh = problem.solve()

    # 5. Calculate L2 Error
    # Exact solution: u(x) = 0.5 * x * (1 - x)
    x_spatial = ufl.SpatialCoordinate(domain)
    u_exact = 0.5 * x_spatial[0] * (1.0 - x_spatial[0])
    
    error_form = fem.form(inner(uh - u_exact, uh - u_exact) * dx)
    l2_error_local = fem.assemble_scalar(error_form)
    l2_error = np.sqrt(domain.comm.allreduce(l2_error_local, MPI.SUM))
    
    return l2_error

# --- Execution ---

element_counts = [4, 8, 16, 32, 64, 128]
degrees = [1, 2, 3, 5, 10]
results = {deg: [] for deg in degrees}

print(f"{'N':<10} | {'Degree':<10} | {'L2 Error':<15}")
print("-" * 40)

for deg in degrees:
    for n in element_counts:
        try:
            error = solve_poisson_problem(n, deg)
            results[deg].append(error)
            print(f"{n:<10} | {deg:<10} | {error:.2e}")
        except Exception as e:
            print(f"Error at N={n}, Deg={deg}: {e}")

# --- Plotting ---

plt.figure(figsize=(8, 6))
for deg in degrees:
    if results[deg]:
        plt.loglog(element_counts[:len(results[deg])], results[deg], '-o', label=f'P{deg} Lagrange')

# Reference slope for Linear elements O(h^2)
h_vals = 1.0 / np.array(element_counts)
plt.loglog(element_counts, (h_vals**2) * (results[1][0] / h_vals[0]**2), 
           'k--', alpha=0.5, label='Theoretical $O(h^2)$')

plt.xlabel('Number of Elements (N)')
plt.ylabel('$L^2$ Error Norm')
plt.title('Convergence Study: $-u\'\' = 1$')
plt.legend()
plt.grid(True, which="both", ls="-", alpha=0.2)
plt.savefig("q1.png", dpi=600)
plt.show()
