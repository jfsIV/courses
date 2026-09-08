import matplotlib.pyplot as plt
import numpy as np
from scipy.sparse.linalg import spsolve
from scipy.sparse import diags


# PART B #######################################################################
def elliptic_solve_1d(f, n):
    x = np.linspace(0, 1, n + 1)[1:-1]
    b = f(x)

    h = 1 / n
    main_diag   =  2 / h**2 * np.ones(n - 1)
    sub_diag    = -1 / h**2 * np.ones(n - 2)
    super_diag  = -1 / h**2 * np.ones(n - 2)

    # format of csr stands for Compressed Sparse Row
    A = diags([main_diag, sub_diag, super_diag], [0, -1, 1], format="csr")
    u = spsolve(A, b)

    return u, x


def f_partB(x):
    t1 = 17 * np.cos(3 * np.pi * x) * np.sin(5 * np.pi * x)
    t2 = 15 * np.sin(3 * np.pi * x) * np.cos(5 * np.pi * x)
    return 2 * np.pi**2 * (t1 + t2)


exact_solution_partB = lambda x: np.cos(3*np.pi*x) * np.sin(5*np.pi*x)


# plotting
def plot_total_partB():
    ks = np.arange(3, 10 + 1)
    ns = 2**ks

    for n in ns:
        u, x = elliptic_solve_1d(f_partB, n)

        x_full = np.concatenate(([0], x, [1]))
        u_full = np.concatenate(([0], u, [0]))

        plt.plot(x_full, u_full, label=f"n={n}")

    exact_x = np.linspace(0, 1, 10_000 + 1)
    exact_u = exact_solution_partB(exact_x)
    plt.plot(exact_x, exact_u, label="Exact")

    plt.legend()
    plt.show()


def plot_single_partB(k, show):
    n = 2**k

    # approximate

    u, x = elliptic_solve_1d(f_partB, n)
    x_full = np.concatenate(([0], x, [1]))
    u_full = np.concatenate(([0], u, [0]))

    plt.plot(x_full, u_full, label=f"k={k}, n={n}")

    # analytical
    exact_x = np.linspace(0, 1, 10_000 + 1)
    exact_u = exact_solution_partB(exact_x)
    plt.plot(exact_x, exact_u, label="Exact", ls="--")

    # plot parameters
    plt.xlabel("x")
    plt.ylabel("u(x)")

    plt.grid(which="both")
    plt.legend()

    plt.savefig(f"hw01_partB_n{n}", dpi=600)
    if show: plt.show()
    plt.close()
    return


# plotting
def do_partB(show):
    plot_single_partB(3,  show)
    plot_single_partB(5,  show)
    plot_single_partB(10, show)

do_partB(show=False)
################################################################################


# PART C #######################################################################
ks = np.arange(3, 10 + 1)
ns = 2**ks

error_ks = []

for n in ns:
    u_approx, x = elliptic_solve_1d(f_partB, n)
    u_exact = exact_solution_partB(x)

    error_norm = np.linalg.norm(u_approx - u_exact, np.inf)
    error_ks.append(error_norm)

error_array = np.array(error_ks)
ratios = error_array[:-1] / error_array[1:]


# latex table
latex_table = "\\begin{table}[h]\n\\centering\n"
latex_table += "\\begin{tabular}{cccc}\n\\hline\n"
latex_table += " $k$ & $n$ & $E_k = \\|\\boldsymbol{u}_k - \\boldsymbol{u}_{\\text{exact}}\\|_\\infty$ & Ratio ($E_{k-1}/E_k$) \\\\\n\\hline\n"

# Loop to populate rows
for i, (k, n, err) in enumerate(zip(ks, ns, error_ks)):
    if i == 0:
        # The first entry (k=3) has no previous error, so ratio is N/A
        latex_table += f" {k} & {n} & {err:.5e} & -- \\\\\n"
    else:
        # Use your ratios array (index shifted by 1 since ratios starts at k=4)
        latex_table += f" {k} & {n} & {err:.5e} & {ratios[i-1]:.5f} \\\\\n"

latex_table += "\\hline\n\\end{tabular}\n"
latex_table += "\\caption{Error analysis and convergence ratios.}\n"
latex_table += "\\label{tab:error_convergence}\n\\end{table}"

print(latex_table)
################################################################################
