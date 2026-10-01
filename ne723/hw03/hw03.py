import matplotlib.pyplot as plt
import matplotlib.colors as colors
import numpy as np
from scipy.optimize import root_scalar

# Question 1 ###################################################################
cs = np.array([
    0.2, 0.5, 0.7, 0.9, 0.97, 0.99, 0.999,
    1.01, 1.03, 1.06, 1.1, 1.15, 1.25, 1.5,
])


def calc_nu0_approx(c):
    # Eq. 2.23 B&G
    c_complex = np.asarray(c, dtype=complex)
    nu0 = (1 / (3 * (1 - c_complex)))**(1/2) * (1 + 2/5 * (1 - c_complex))
    nu0_mod = np.abs(nu0)
    return nu0_mod


def _calc_nu0_exact(c):
    # Eq. 2.20 B&G
    if c == 1:
        return float("inf")

    if c < 1:
        f = lambda nu0: c * nu0 * np.arctanh(1 / nu0) - 1
        guess = max(1.0001, calc_nu0_approx(c))

        solution = root_scalar(f, x0=guess, fprime=None, method="secant")
        return abs(solution.root)

    if c > 1:
        f = lambda eta0: c * eta0 * np.arctan(1 / eta0) - 1
        guess = calc_nu0_approx(c)

        solution = root_scalar(f, x0=guess, fprime=None, method="secant")
        return abs(solution.root)


calc_nu0_exact = np.vectorize(_calc_nu0_exact, otypes=[float])


def calc_L(c):
    # Eq. 2.24 B&G
    c_complex = np.asarray(c, dtype=complex)
    L = (1 / (3 * (1 - c_complex)))**(1/2)
    L_mod = np.abs(L)
    return L_mod


def solve_q1():
    nu0s_exact = calc_nu0_exact(cs)
    nu0s_approx = calc_nu0_approx(cs)
    Ls = calc_L(cs)

    diffs = Ls - nu0s_exact
    one_minus_ratios = 1 - Ls/nu0s_exact

    title_line = f"{"c":<5} | {"nu0 exact"} | {"nu0 approx":<8} | {"L":<8} | {"diff":<8} | {"1-ratio":<7}"
    print(title_line)
    print("-" * 62)


    for c, nu0_exact, nu0_approx, L, diff, one_minus_ratio in zip(cs, nu0s_exact, nu0s_approx, Ls, diffs, one_minus_ratios):
        str1 = f"{c:<6}"
        str2 = f"{nu0_exact:<10.5f}"
        str3 = f"{nu0_approx:<11.5f}"
        str4 = f"{L:<9.5f}"
        str5 = f"{diff:<9.5f}"
        str6 = f"{one_minus_ratio:<8.5f}"

        gap = "| "
        print(str1 + gap + str2 + gap + str3 + gap + str4 + gap + str5 + gap + str6)


solve_q1()
# end Question 1 ###############################################################

# Question 2 ###################################################################
# givens
c_a = 0.9
c_b = 1.1

# plot settings
plot_part_a = False
plot_part_b = False

# dikscretizing space
n_x_points = 2000
n_mu_points = 200

xs = np.linspace(-10, 10, n_x_points)
mus = np.linspace(-1, 1, n_mu_points)

xs, mus = np.meshgrid(xs, mus)


# part a -----------------------------------------------------------------------
def partA_psi_plus(x, mu, nu0=calc_nu0_exact(c_a)):
    psi_plus = c_a * nu0 / (2 * (nu0 - mu)) * np.exp(-x / nu0)
    return psi_plus


def partA_psi_minus(x, mu, nu0=calc_nu0_exact(c_a)):
    psi_minus = c_a * nu0 / (2 * (nu0 + mu)) * np.exp(x / nu0)
    return psi_minus


# plus mode
psi_plus_a = partA_psi_plus(xs, mus)
plt.contourf(
        xs, mus, psi_plus_a,
        norm=colors.LogNorm(), levels=20, cmap="viridis"
)

plt.colorbar(label=r'$\psi_0^+(x, \mu)$')
plt.xlabel('$x$')
plt.ylabel(r'$\mu$')

if plot_part_a: plt.savefig("hw03_q2a_plus.png", dpi=600)
plt.close()

# minus mode
psi_minus_a = partA_psi_minus(xs, mus)
plt.contourf(
        xs, mus, psi_minus_a,
        norm=colors.LogNorm(), levels=20, cmap="viridis"
)

plt.colorbar(label=r'$\psi_0^-(x, \mu)$')
plt.xlabel('$x$')
plt.ylabel(r'$\mu$')

if plot_part_a: plt.savefig("hw03_q2a_minus.png", dpi=600)
plt.close()
# end part a -------------------------------------------------------------------


# part b -----------------------------------------------------------------------
def partB_psi_plus_real(x, mu, eta0=calc_nu0_exact(c_b)):
    term1 = c_b * eta0**2 / (2 * (eta0**2 + mu**2)) * np.cos(x / eta0)
    term2 = c_b * eta0 * mu / (2 * (eta0**2 + mu**2)) * np.sin(x / eta0)
    return term1 + term2


def partB_psi_plus_imaginary(x, mu, eta0=calc_nu0_exact(c_b)):
    term1 = c_b * eta0**2 / (2 * (eta0**2 + mu**2)) * np.sin(x / eta0)
    term2 = -c_b * eta0 * mu / (2 * (eta0**2 + mu**2)) * np.cos(x / eta0)
    return term1 + term2


def partB_psi_minus_real(x, mu, eta0=calc_nu0_exact(c_b)):
    term1 = c_b * eta0**2 / (2 * (eta0**2 + mu**2)) * np.cos(x / eta0)
    term2 = c_b * eta0 * mu / (2 * (eta0**2 + mu**2)) * np.sin(x / eta0)
    return term1 + term2


def partB_psi_minus_imaginary(x, mu, eta0=calc_nu0_exact(c_b)):
    term1 = c_b * eta0 * mu / (2 * (eta0**2 + mu**2)) * np.cos(x / eta0)
    term2 = -c_b * eta0**2 / (2 * (eta0**2 + mu**2)) * np.sin(x / eta0)
    return term1 + term2


# plus mode, real
psi_plus_b_real = partB_psi_plus_real(xs, mus)
plt.contourf(
        xs, mus, psi_plus_b_real,
        levels=30, cmap="viridis"
)

plt.colorbar(label=r'$\psi_0^+(x, \mu)$')
plt.xlabel('$x$')
plt.ylabel(r'$\mu$')

if plot_part_b: plt.savefig("hw03_q2b_plus_real.png", dpi=600)
plt.close()


# plus mode, imaginary
psi_plus_b_imaginary = partB_psi_plus_imaginary(xs, mus)
plt.contourf(
        xs, mus, psi_plus_b_imaginary,
        levels=30, cmap="viridis"
)

plt.colorbar(label=r'$\psi_0^+(x, \mu)$')
plt.xlabel('$x$')
plt.ylabel(r'$\mu$')

if plot_part_b: plt.savefig("hw03_q2b_plus_imaginary.png", dpi=600)
plt.close()


# minus mode, real
psi_minus_b_real = partB_psi_minus_real(xs, mus)
plt.contourf(
        xs, mus, psi_minus_b_real,
        levels=30, cmap="viridis"
)

plt.colorbar(label=r'$\psi_0^+(x, \mu)$')
plt.xlabel('$x$')
plt.ylabel(r'$\mu$')

if plot_part_b: plt.savefig("hw03_q2b_minus_real.png", dpi=600)
plt.close()


# minus mode, imaginary
psi_minus_b_imaginary = partB_psi_minus_imaginary(xs, mus)
plt.contourf(
        xs, mus, psi_minus_b_imaginary,
        levels=30, cmap="viridis"
)

plt.colorbar(label=r'$\psi_0^+(x, \mu)$')
plt.xlabel('$x$')
plt.ylabel(r'$\mu$')

if plot_part_b: plt.savefig("hw03_q2b_minus_imaginary.png", dpi=600)
plt.close()
# end part b -------------------------------------------------------------------

# end Question 2 ###############################################################
