a = 13
b = 3
M = 16

def rng(x):
    print(x)
    x = (a * x + b) % M
    rng(x)


rng(5)
