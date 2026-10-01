import numpy as np
from random import random
import time


# problem setup
# - 3D sphere geometry
# - point source in at (x0,y0,z0)=(0,0,0)
# - Sigma_a = 0.1, Sigma_s = 0.2
# - R = 20cm

R = 20

Sigma_a = 0.1
Sigma_s = 0.2
Sigma_t = Sigma_a + Sigma_s


# sampling functions
sample_mu = lambda eta: 2 * eta - 1
sample_phi = lambda eta: 2 * np.pi * eta
sample_path_length = lambda eta: -np.log(eta) / Sigma_t


# looping
def calc_abs_scat_prob(n_samples):
    start_time = time.perf_counter()
    n_leaked = 0

    for n_sample in range(n_samples):
        eta1 = random()
        x0, y0, z0 = 0, 0, 0

        while True:
            eta2, eta3, eta4 = random(), random(), random()

            mu = sample_mu(eta2)
            phi = sample_phi(eta3)
            path_length = sample_path_length(eta4)

            xf = x0 + path_length * (1 - mu**2)**(1/2) * np.cos(phi)
            yf = y0 + path_length * (1 - mu**2)**(1/2) * np.sin(phi)
            zf = z0 + path_length * mu

            rf = (xf**2 + yf**2 + zf**2)**(1/2)

            # break if leaked
            if rf >= R:
                n_leaked += 1
                break

            # break if absorbed
            eta5 = random()
            if Sigma_s / Sigma_t < eta5:
                break

            x0, y0, z0 = xf, yf, zf

    leakage_prob = n_leaked / n_samples
    absorption_prob = 1 - leakage_prob

    # output
    end_time = time.perf_counter()
    runtime = end_time - start_time

    print("\n" + f"----- N = {n_samples:.0e} -----")
    print(f"time: {runtime:.5f} s")
    print(f"leakage probability: {leakage_prob:.5%}")
    print(f"absorption probability: {absorption_prob:.5%}")

    return 


for n_samples in [10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000]:
    calc_abs_scat_prob(n_samples)
