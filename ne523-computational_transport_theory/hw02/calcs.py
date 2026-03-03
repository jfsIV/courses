import matplotlib.pyplot as plt
import numpy as np
from scipy.integrate import quad

# Legendre polynomials
p0 = lambda x : (1/2)**(1/2) + x * 0
p1 = lambda x : (3/2)**(1/2) * x
p2 = lambda x : (5/2)**(1/2) * ((3*x**2 - 1)/2)

# functions
fa = lambda x : 3 * x + 7
fb = lambda x : np.exp(x)
fc = lambda x : np.exp(5 * x)

# coefficients
## part a
fa0 = 7 * 2**(1/2)
fa1 = 6**(1/2)
fa2 = 0

## part b
fb0 = 1 / 2**(1/2) * (np.exp(1) - np.exp(-1))
fb1 = np.sqrt(6) * np.exp(-1)
fb2 = np.sqrt(5/2) * (np.exp(1) - 7 * np.exp(-1))

## part c
fc0 = 1 / 5 / 2**(1/2) * (np.exp(5) - np.exp(-5))
fc1 = np.sqrt(6) / 26 * (2 * np.exp(5) - 3 * np.exp(-5))
fc2 = 1 / 5**(5/2) / 2**(1/2) * (13 * np.exp(5) - 43 * np.exp(-5))


# plotting
def plot_func(f=fa, c0=fa0, c1=fa1, c2=fa2, name="fa"):
    f0 = lambda x : c0 * p0(x)
    f1 = lambda x : c1 * p1(x) + f0(x)
    f2 = lambda x : c2 * p2(x) + f1(x)

    xs = np.linspace(-1, 1, 1000)

    # plotting
    fig, ax = plt.subplots()

    ax.plot(xs, f(xs), label="Exact Solution", lw=2)
    ax.plot(xs, f0(xs), label="$\\tilde{f},\quad i=0$", ls="dashed", lw=2)
    ax.plot(xs, f1(xs), label="$\\tilde{f},\quad i=1$", ls="dashed", lw=2)
    ax.plot(xs, f2(xs), label="$\\tilde{f},\quad i=2$", ls="dashed", lw=2)

    ax.legend()
    ax.grid(which="both")

    ax.set_xlabel("x position")
    ax.set_ylabel("f(x) value")

    plt.savefig(f"{name}.png", dpi=600)
    plt.show()


# saving 
plot_func(fa, fa0, fa1, fa2, "fa")
plot_func(fb, fb0, fb1, fb2, "fb")
plot_func(fc, fc0, fc1, fc2, "fc")
