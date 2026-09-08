import numpy as np
from mpi4py import MPI
import dolfinx
from dolfinx import mesh, fem, plot, io, default_scalar_type
from dolfinx.fem.petsc import LinearProblem
import ufl
import pyvista
from pathlib import Path

# 1. Print Version Info
print(f"DOLFINx version: {dolfinx.__version__} based on GIT commit: {dolfinx.git_commit_hash}")

# 2. Create Mesh
# Create a unit square mesh with 8x8 triangles
domain = mesh.create_unit_square(MPI.COMM_WORLD, 8, 8, mesh.CellType.triangle)

# 3. Define Function Space
# Using Lagrange elements of degree 1
V = fem.functionspace(domain, ("Lagrange", 1))

# 4. Define Dirichlet Boundary Conditions
uD = fem.Function(V)
uD.interpolate(lambda x: 1 + x[0]**2 + 2 * x[1]**2)

# Create facet to cell connectivity required to determine boundary facets
tdim = domain.topology.dim
fdim = tdim - 1
domain.topology.create_connectivity(fdim, tdim)
boundary_facets = mesh.exterior_facet_indices(domain.topology)

boundary_dofs = fem.locate_dofs_topological(V, fdim, boundary_facets)
bc = fem.dirichletbc(uD, boundary_dofs)

# 5. Define Variational Problem
u = ufl.TrialFunction(V)
v = ufl.TestFunction(V)

# Source term f = -6
f = fem.Constant(domain, default_scalar_type(-6))

a = ufl.dot(ufl.grad(u), ufl.grad(v)) * ufl.dx
L = f * v * ufl.dx

# 6. Solve Problem
problem = LinearProblem(
    a,
    L,
    bcs=[bc],
    petsc_options={"ksp_type": "preonly", "pc_type": "lu"},
    petsc_options_prefix="Poisson",
)
uh = problem.solve()

# 7. Error Computation
# Compute L2 error against an exact solution interpolated on a higher-order space
V2 = fem.functionspace(domain, ("Lagrange", 2))
uex = fem.Function(V2, name="u_exact")
uex.interpolate(lambda x: 1 + x[0] ** 2 + 2 * x[1] ** 2)

L2_error = fem.form(ufl.inner(uh - uex, uh - uex) * ufl.dx)
error_local = fem.assemble_scalar(L2_error)
error_L2 = np.sqrt(domain.comm.allreduce(error_local, op=MPI.SUM))

error_max = np.max(np.abs(uD.x.array - uh.x.array))

if domain.comm.rank == 0:
    print(f"Error_L2 : {error_L2:.2e}")
    print(f"Error_max : {error_max:.2e}")

# 8. Visualization with PyVista
if pyvista.OFF_SCREEN:
    pyvista.start_xvfb()

# Create PyVista grid for the domain
domain.topology.create_connectivity(tdim, tdim)
topology, cell_types, geometry = plot.vtk_mesh(domain, tdim)
grid = pyvista.UnstructuredGrid(topology, cell_types, geometry)
plotter0 = pyvista.Plotter()
plotter0.add_mesh(grid, show_edges=True)
plotter0.view_xy()
if not pyvista.OFF_SCREEN:
    plotter0.show()

# Create PyVista grid for the solution
u_topology, u_cell_types, u_geometry = plot.vtk_mesh(V)
u_grid = pyvista.UnstructuredGrid(u_topology, u_cell_types, u_geometry)
u_grid.point_data["u"] = uh.x.array.real
u_grid.set_active_scalars("u")

# Warp by scalar to see 3D elevation
warped = u_grid.warp_by_scalar()

# Visualization Plot
plotter1 = pyvista.Plotter()
plotter1.add_mesh(warped, show_edges=True, show_scalar_bar=True)
if not pyvista.OFF_SCREEN:
    plotter1.show()

# 9. Save Results to File
results_folder = Path("results/poisson_dirichlet")
results_folder.mkdir(exist_ok=True, parents=True)
filename = results_folder / "fundamentals"

plotter0.screenshot(f"{results_folder}/mesh.png")
plotter1.screenshot(f"{results_folder}/poisson_solution.png")

# Save as VTX (ADIOS2)
with io.VTXWriter(domain.comm, filename.with_suffix(".bp"), [uh]) as vtx:
    vtx.write(0.0)

# Save as XDMF
with io.XDMFFile(domain.comm, filename.with_suffix(".xdmf"), "w") as xdmf:
    xdmf.write_mesh(domain)
    xdmf.write_function(uh)
