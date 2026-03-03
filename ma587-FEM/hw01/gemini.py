import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import simpson
from scipy.linalg import solve

# ==========================================
# Part (a): Analytical Functions
# ==========================================
def u_exact(x):
    return np.sin(np.pi * x)**2

def f_analytical(x):
    return -8 * (np.pi**4) * np.cos(2 * np.pi * x)

# ==========================================
# Part (b): Basis Functions from Screenshot 1
# ==========================================
class HermiteBasis:
    def __init__(self, a, b):
        self.a, self.b = a, b
        # Constants for phi_L and phi_R
        denom_phi = (a**3 - b**3)/3 - (a+b)/2 * (a**2 - b**2) + a*b*(a-b)
        self.A1 = 1.0 / denom_phi
        self.C1 = -self.A1 * (b**3/3 - (a+b)/2 * b**2 + a*b**2)
        
        self.A2 = -self.A1 # From symmetry/screenshot 2b
        self.C2 = -self.A2 * (a**3/3 - (a+b)/2 * a**2 + a**2*b)
        
        # Constants for psi_L and psi_R
        self.A3 = (a - b)**(-2)
        self.A4 = self.A3

    def phi_L(self, x):
        return self.A1 * (x**3/3 - (x**2/2)*(self.a + self.b) + x*self.a*self.b) + self.C1
    
    def phi_R(self, x):
        return self.A2 * (x**3/3 - (x**2/2)*(self.a + self.b) + x*self.a*self.b) + self.C2

    def psi_L(self, x):
        return self.A3 * (x - self.b)**2 * (x - self.a)

    def psi_R(self, x):
        return self.A4 * (x - self.a)**2 * (x - self.b)

    # Second Derivatives
    def phi_L_pp(self, x):
        return self.A1 * (2*x - (self.a + self.b))

    def phi_R_pp(self, x):
        return self.A2 * (2*x - (self.a + self.b))

    def psi_L_pp(self, x):
        return self.A3 * (6*x - 4*self.b - 2*self.a)

    def psi_R_pp(self, x):
        return self.A4 * (6*x - 4*self.a - 2*self.b)

# ==========================================
# Main FEM Solver
# ==========================================
def run_fem(M):
    h = 1.0 / M
    nodes = np.linspace(0, 1, M + 1)
    
    # Total DOFs: 2 per node (value and slope). 
    # Node 0 and Node M are fixed (4 constraints), so internal DOFs = 2*(M-1)
    num_total_dofs = 2 * (M + 1)
    K_global = np.zeros((num_total_dofs, num_total_dofs))
    F_global = np.zeros(num_total_dofs)
    
    for i in range(M):
        a, b = nodes[i], nodes[i+1]
        basis = HermiteBasis(a, b)
        
        # Quadrature setup for the element
        x_q = np.linspace(a, b, 100)
        funcs = [basis.phi_L, basis.phi_R, basis.psi_L, basis.psi_R]
        derivs = [basis.phi_L_pp, basis.phi_R_pp, basis.psi_L_pp, basis.psi_R_pp]
        
        # Global DOF mapping: Node i -> (2*i, 2*i+1)
        # Sequence in screenshot 2: c1=phi_L, c2=phi_R, c3=psi_L, c4=psi_R
        map_idx = [2*i, 2*(i+1), 2*i+1, 2*(i+1)+1]
        
        # Part (c) & (d): Local Stiffness and Load assembly
        for r in range(4):
            # Load vector b using Simpson's
            F_global[map_idx[r]] += simpson(y=f_analytical(x_q) * funcs[r](x_q), x=x_q)
            
            for c in range(4):
                # Stiffness matrix A
                K_val = simpson(y=derivs[r](x_q) * derivs[c](x_q), x=x_q)
                K_global[map_idx[r], map_idx[c]] += K_val

    # Apply BCs: u(0)=u'(0)=u(1)=u'(1)=0
    # Boundary DOF indices: 0, 1 (node 0) and 2*M, 2*M+1 (node M)
    free_indices = np.arange(2, 2*M)
    K_reduced = K_global[np.ix_(free_indices, free_indices)]
    F_reduced = F_global[free_indices]
    
    # Part (e): Solve and Reconstruct
    xi_star = solve(K_reduced, F_reduced)
    full_xi = np.zeros(num_total_dofs)
    full_xi[free_indices] = xi_star
    
    def u_h(x_eval):
        # Determine which element x falls into
        if x_eval >= 1.0: x_eval = 1.0 - 1e-12
        idx = int(x_eval // h)
        a, b = nodes[idx], nodes[idx+1]
        local_basis = HermiteBasis(a, b)
        # coeffs: phi_L, phi_R, psi_L, psi_R
        c = [full_xi[2*idx], full_xi[2*(idx+1)], full_xi[2*idx+1], full_xi[2*(idx+1)+1]]
        return c[0]*local_basis.phi_L(x_eval) + c[1]*local_basis.phi_R(x_eval) + \
               c[2]*local_basis.psi_L(x_eval) + c[3]*local_basis.psi_R(x_eval)
    
    return np.vectorize(u_h)

# ==========================================
# Part (f): Error Analysis and Plotting
# ==========================================
M_steps = np.arange(5, 105, 5)
rel_errors = []
x_fine = np.linspace(0, 1, int(1e5))
u_fine_exact = u_exact(x_fine)
exact_l2 = np.sqrt(np.trapezoid(u_fine_exact**2, x_fine))

print(f"{'M':<5} | {'Relative L2 Error':<20}")
print("-" * 30)

for M in M_steps:
    uh_func = run_fem(M)
    u_approx = uh_func(x_fine)
    
    l2_error = np.sqrt(np.trapezoid((u_fine_exact - u_approx)**2, x_fine))
    rel_error = l2_error / exact_l2
    rel_errors.append(rel_error)
    print(f"{M:<5} | {rel_error:<20.4e}")

# Plotting
plt.figure(figsize=(8, 5))
plt.loglog(M_steps, rel_errors, 'o-', label='FEM Error')
# Theoretical convergence O(h^4) for L2 norm with cubic Hermite
plt.loglog(M_steps, 0.1 * (M_steps/5)**-4, '--', label='Order 4 Reference')
plt.title("Relative $L^2$ Error vs. Number of Elements")
plt.xlabel("M (Elements)")
plt.ylabel("Relative Error")
plt.grid(True, which="both", ls="-", alpha=0.5)
plt.legend()
plt.savefig("gemini.png", dpi=600)
plt.show()
