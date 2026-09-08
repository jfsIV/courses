import numpy as np
import matplotlib.pyplot as plt

# question 1
def question1():
    # parameters
    lam  = 0.08
    beta = 0.0065
    Lam  = 0.001
    rhos = np.array([0.0011, 0.0022, 0.0044, 0.0001])

    alpha = (beta - rhos + lam * Lam)

    omega1s = -alpha + (alpha**2 + 4 * Lam * lam * rhos)**(1/2)
    omega2s = -alpha - (alpha**2 + 4 * Lam * lam * rhos)**(1/2)


    # functions
    def analytical(case):
        omega1 = omega1s[case]
        omega2 = omega2s[case]
        rho    = rhos[case]

        t1 = lambda t: (rho/Lam - omega2) * np.exp(omega1 * t)
        t2 = lambda t: (omega1 - rho/Lam) * np.exp(omega2 * t)

        return lambda t: 1 / (omega1 - omega2) * (t1(t) + t2(t))


    def approximate(case):
        rho = rhos[case]

        t1 = lambda t: beta/(beta - rho) * np.exp((lam * rho) / (beta - rho) * t)
        t2 = lambda t: -rho/(beta - rho) * np.exp((-beta + rho) / Lam * t)

        return lambda t: t1(t) + t2(t)


    # plotting
    for i in range(len(rhos)):
        t = np.linspace(0, 0.25)

        ana = analytical(i)
        app = approximate(i)

        plt.plot(t, ana(t), label="Analytical Solution")
        plt.plot(t, app(t), label="Approximate Solution")

        plt.xlabel("Time  [s]")
        plt.ylabel("Relative n$^0$ Population")

        plt.title(f"Case {i + 1}")
        plt.grid(which="both")

        plt.legend()
        plt.savefig(f"question1-case{i}", dpi=600)
        plt.close()


def question2():
    beta = 0.0075
    Lam = 1e-5
    rho_1 = 1.1 * beta

    rho_p1 = 0.1 * beta
    delta_t = 4 * Lam / rho_p1

    gamma = -0.8
    gamma_e = 0.006
    p_0 = 1

    psuedo_p = rho_1 / rho_p1 * p_0
    p_m = psuedo_p - rho_p1**2 / 2 / Lam / gamma_e

    delta_t = 4 * Lam / rho_p1 * (1 - p_0 / p_m)
    Q_t2 = -2 * 0.1 / gamma

    print("----- Question 2 -----")
    print("  Delta t: ", round(delta_t * 1000, 1), "ms")
    print("  Q(t2)  : ", Q_t2, " fp-s")

    # part a
    print("part a)")
    lam_h = 0.5
    per_err = np.exp(lam_h * delta_t) - 1
    print("  Percent Error: ", round(per_err * 100, 3), "%")

    # part b
    print("part b)")
    print("  Energy removing cooling: ", round(delta_t, 5), "fp-s")
    print("  ratio: ", round(delta_t / Q_t2, 5))

def question3():
    # parameters
    u235_lam   = 0.4
    u235_gamma = -0.16
    u235_beta  = 0.00650

    mox_lam   = 0.6
    mox_gamma = -0.08
    mox_beta  = 0.00350

    # solving
    u235_rho00 = -u235_lam / u235_gamma  # Eq. II.61
    mox_rho00 = -mox_lam / mox_gamma

    print("----- Question 3 -----")
    print("  U235 rho00: ", u235_rho00)
    print("  MOX rho00:  ", mox_rho00)


def question4():
    # parameters [$]
    p_n = 1  # comparing relative to p_n
    p_0 = 0.1 * p_n
    gamma = -0.08
    beta_over_Lam = 0.62e4

    beta = 1
    Lam = beta / beta_over_Lam

    rho_1 = 1.1  # assumed
    rho_p1 = rho_1 - beta  # II.78

    t_m = 16.5 * Lam / rho_p1
    psuedo_p = rho_1 / (rho_1 - beta) * p_0  # II.75
    p_m = psuedo_p - rho_p1**2 / (2 * Lam * gamma)  # II.95
    rho_b = (rho_p1**2 - 2 * Lam * gamma * psuedo_p)**(1/2)  # II.100b

    # calculations
    p_t = lambda t: p_m / (np.cosh(rho_b / 2 / Lam * (t - t_m)))**2

    # plotting
    t = np.linspace(0, 0.05, 250)

    plt.plot(t, p_t(t), label="Relative Power", color="b")

    plt.axhline(p_m, label=f"Max Power {round(p_m, 4)}$p_n$", color="r", ls="dashed")
    plt.axvline(t_m, label=f"Max Power time {round(t_m, 5)} s", color="k", ls="dashed")

    plt.xlabel("Time  [s]")
    plt.ylabel("Relative Power p(t)/$p_n$")

    plt.grid(which="both")
    plt.legend()

    plt.savefig("question4.png", dpi=600)
    plt.close()


#question1()
question2()
#question3()
#question4()
