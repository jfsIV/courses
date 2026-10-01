import numpy as np
from random import random
import time


# problem setup
# - slab geometry from (0 <= x <= H=10cm)
# - uniform source distribution
# - Sigma_a = Sigma_s = 0.5 cm^-1

H = 10

Sigma_a = 0.5
Sigma_s = 0.5
Sigma_t = Sigma_a + Sigma_s


# sampling function for each variable
sample_x_initial = lambda eta: H * eta
sample_mu = lambda eta: 2 * eta - 1
sample_path_length = lambda eta: -np.log(eta) / Sigma_t


# looping
def calc_abs_scat_prob(n_samples):
    start_time = time.perf_counter()
    n_leaked = 0

    for n_sample in range(n_samples):
        eta1 = random()
        x_initial = sample_x_initial(eta1)

        while True:
            eta2, eta3 = random(), random()

            mu = sample_mu(eta2)
            path_length = sample_path_length(eta3)

            x_final = x_initial + mu * path_length

            # break if leaked
            if x_final < 0 or x_final > H:
                n_leaked += 1
                break

            # break if absorbed
            eta4 = random()
            if Sigma_s / Sigma_t < eta4:
                break

            x_initial = x_final

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


for n_samples in [10, 100, 1_000, 10_000, 100_000, 1_000_000]:
    calc_abs_scat_prob(n_samples)
