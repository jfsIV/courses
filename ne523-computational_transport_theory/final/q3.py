import numpy as np
import matplotlib.pyplot as plt

# values
dd1 = np.array([0.453403,  0.454454,  0.454541])
dd2 = np.array([0.0797003, 0.0825982, 0.0828288])
dd3 = np.array([0.197327,  0.218892,  0.222921])

ex1 = 0.454590
ex2 = 0.0829595
ex3 = 0.228800

cs = 1 / np.array([10, 30, 50])

# error calcs
err1 = np.abs(dd1 - ex1)
err2 = np.abs(dd2 - ex2)
err3 = np.abs(dd3 - ex3)


# plotting
plt.plot(1/cs, err1, marker="o", label="(0.35,0.65)")
plt.plot(1/cs, err2, marker="o", label="(0.05,0.95)")
plt.plot(1/cs, err3, marker="o", label="(0.15,0.15)")

plt.grid(which="both")

plt.xscale("log")
plt.yscale("log")

plt.legend()

plt.ylabel("Magnitude of Error b/w the DD and Exact Solutions")
plt.xlabel("Reciprocal of Cell Size or the Number of Cells")
plt.savefig("final-q3.png", dpi=600)
plt.close()

# printing
def pprint(err):
    e1 = round(err[0], 6)
    e2 = round(err[0], 7)
    e3 = round(err[0], 6)
    
    print(f"{e1} & {e2} & {e3} \\\\")

print("Part c")
print("------")
pprint(err1)
pprint(err2)
pprint(err3)


print("\nPart d")
print("------")

def calc_order(err):
    err_ratio = err[1] / err[-1]
    cs_ratio = (1/30) / (1/50)

    order = np.log(err_ratio) / np.log(cs_ratio)
    print(round(order, 5))

calc_order(err1)
calc_order(err2)
calc_order(err3)
