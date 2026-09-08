import numpy as np
import matplotlib.pyplot as plt

# parameters
x0 = 1
C = 1e-6
h = 0.1
tf = 0.5

# equations
def exact_equation(t):
    return np.exp(C * t) + np.sin(t)



# solving
ts = np.arange(0, tf + h, h)

xbes = np.zeros_like(ts)
xcns = np.zeros_like(ts)
xs = exact_equation(ts)

for i, t in enumerate(ts):
    # initial condition
    if i == 0:
        xbes[0] = x0
        xcns[0] = x0
        continue

    # backwards euler
    xbes[i] = (xbes[i-1] - h*C*np.sin(t) + h*np.cos(t)) / (1 - h*C)

    # crank-nicolson
    a1 = (1 + C*h/2) * xcns[i-1]
    a2 = -(C*h/2) * (np.sin(t-h) + np.sin(t))
    a3 = h/2 * (np.cos(t-h) + np.cos(t))
    xcns[i] = (a1 + a2 + a3) / (1 - h*C/2)

be_err = (xbes - xs) / abs(xs)
cn_err = (xcns - xs) / abs(xs)

# plotting
plt.plot(ts, xbes - xs, label="Backward Euler")
plt.plot(ts, xcns - xs, label="Crank-Nicolson")

plt.xlabel("Time [s]")
plt.ylabel("Relative Error, (Approx-Exact)/abs(Exact)")

plt.legend()
plt.grid(which="both")

plt.savefig("q1b.png", dpi=600)
plt.close()


# printing
for i in range(len(ts)):
    t = ts[i]
    be = be_err[i]
    cn = cn_err[i]

    if i == 0:
        print("0 & 0 & 0 \\\\")
        continue

    print(f"{t:.1} & {be:.5e} & {cn:.5e} \\\\")
