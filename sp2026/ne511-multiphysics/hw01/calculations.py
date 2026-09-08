import numpy as np

# print booleans
print_q1 = 0
print_q3 = 1

# Problem 1
beta_u235  = 0.00650
beta_pu239 = 0.00210

nu_u235 = 2.43
nu_pu239 = 2.90

prompt_lifetime = 1e-4

q1_lambdas = np.array([
    0.012375, 0.030130, 0.111774, 0.301304, 1.136065, 3.013043])

q1_u235_betas = np.array([
    0.00021, 0.00142, 0.00128, 0.00257, 0.00075, 0.00027])

q1_pu239_betas = np.array([
    0.00007, 0.00063, 0.00044, 0.00069, 0.00018, 0.00009])


## calculate DNF
def q1_calc_dnf(p_u235):
    """calculates the delayed neutron fraction from the fraction of power from U235"""

    if (p_u235 > 1): raise ValueError("fraction from (0,1)")
    if (p_u235 < 0): raise ValueError("fraction cannot be negative")

    p_pu239 = 1 - p_u235

    # power is proportional to number of fissions
    nu_d = beta_u235 * nu_u235 * p_u235 + beta_pu239 * nu_pu239 * p_pu239
    nu_t = nu_u235 * p_u235 + nu_pu239 * p_pu239

    return(nu_d / nu_t)


q1_betas = np.array([
    q1_calc_dnf(0.90),
    q1_calc_dnf(0.85),
    q1_calc_dnf(0.80),
    q1_calc_dnf(0.70)
])

q1_percent_change = (q1_betas - q1_betas[0]) / q1_betas[0]

#for beta in q1_betas: print(beta)

## calculate core average neutron lifetime
def q1_calc_lifetime(p_u235):
    p_pu239 = 1 - p_u235
    lt_dgs = prompt_lifetime + 1 / q1_lambdas
    #print(lt_dgs) agree

    lt_p_u = (1 - beta_u235) * prompt_lifetime
    lt_p_pu = (1 - beta_pu239) * prompt_lifetime

    lt_d_u = sum(q1_u235_betas * lt_dgs)
    lt_d_pu = sum(q1_pu239_betas * lt_dgs)

    lt_u = lt_p_u + lt_d_u
    lt_pu = lt_p_pu + lt_d_pu

    top = (p_u235 * nu_u235 * lt_u + p_pu239 * nu_pu239 * lt_pu)
    bot = (p_u235 * nu_u235 + p_pu239 * nu_pu239)

    return(top / bot)


q1_lts = np.array([
    q1_calc_lifetime(0.90),
    q1_calc_lifetime(0.85),
    q1_calc_lifetime(0.80),
    q1_calc_lifetime(0.70),
])


## printing
if print_q1:
    print("\n----- Problem 1 -----")
    print("Betas:")

    for i in range(len(q1_betas)):
        beta = round(q1_betas[i], 5)
        percent_change = round(100 * q1_percent_change[i], 3)

        match i:
            case 0: print(f"BOC & 90\\% & {beta} & {percent_change}\\% \\\\")
            case 1: print(f"MOC1 & 85\\% & {beta} & {percent_change}\\% \\\\")
            case 2: print(f"MOC2 & 80\\% & {beta} & {percent_change}\\% \\\\")
            case 3: print(f"EOC & 70\\% & {beta} & {percent_change}\\% \\\\ \\hline")

    print("\nLifetimes: ")
    for i in range(len(q1_lts)):
        lifetime = round(q1_lts[i], 5)

        match i:
            case 0: print(f"BOC & 90\\% & {lifetime} \\\\")
            case 1: print(f"MOC1 & 85\\% & {lifetime} \\\\")
            case 2: print(f"MOC2 & 80\\% & {lifetime} \\\\")
            case 3: print(f"EOC & 70\\% & {lifetime} \\\\ \\hline")
    
    print("")

# Problem 3
# I asked gemini to convert this for me
q3_data = {
    1: {"ug": 0.5, "Eg": 6.065e6, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0.0325},
    2: {"ug": 1.0, "Eg": 3.679e6, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0.1217},
    3: {"ug": 1.5, "Eg": 2.231e6, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0.2109},
    4: {"ug": 2.0, "Eg": 1.353e6, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0.009, "chi": 0.2230},
    5: {"ug": 2.5, "Eg": 0.821e6, "chi1": 0.009, "chi2": 0.022, "chi3": 0.012, "chi456": 0.025, "chi": 0.1728},
    6: {"ug": 3.0, "Eg": 0.498e6, "chi1": 0.021, "chi2": 0.066, "chi3": 0.070, "chi456": 0.062, "chi": 0.1105},
    7: {"ug": 3.5, "Eg": 0.302e6, "chi1": 0.093, "chi2": 0.295, "chi3": 0.191, "chi456": 0.184, "chi": 0.0628},
    8: {"ug": 4.0, "Eg": 0.183e6, "chi1": 0.088, "chi2": 0.110, "chi3": 0.094, "chi456": 0.128, "chi": 0.0316},
    9: {"ug": 4.5, "Eg": 0.111e6, "chi1": 0.156, "chi2": 0.098, "chi3": 0.097, "chi456": 0.157, "chi": 0.0168},
    10: {"ug": 5.0, "Eg": 0.674e5, "chi1": 0.174, "chi2": 0.108, "chi3": 0.156, "chi456": 0.109, "chi": 0.0083},
    11: {"ug": 5.5, "Eg": 0.409e5, "chi1": 0.171, "chi2": 0.107, "chi3": 0.142, "chi456": 0.109, "chi": 0.0040},
    12: {"ug": 6.0, "Eg": 0.248e5, "chi1": 0.131, "chi2": 0.088, "chi3": 0.118, "chi456": 0.099, "chi": 0.0019},
    13: {"ug": 6.5, "Eg": 0.150e5, "chi1": 0.121, "chi2": 0.079, "chi3": 0.080, "chi456": 0.089, "chi": 0.0009},
    14: {"ug": 7.0, "Eg": 0.912e4, "chi1": 0.020, "chi2": 0.013, "chi3": 0.015, "chi456": 0.015, "chi": 0.0004},
    15: {"ug": 7.5, "Eg": 0.553e4, "chi1": 0.010, "chi2": 0.009, "chi3": 0.012, "chi456": 0.010, "chi": 0.0002},
    16: {"ug": 8.0, "Eg": 0.335e4, "chi1": 0.005, "chi2": 0.004, "chi3": 0.005, "chi456": 0.004, "chi": 0.0001},
    17: {"ug": 8.5, "Eg": 0.203e4, "chi1": 0.001, "chi2": 0.001, "chi3": 0, "chi456": 0, "chi": 0},
    18: {"ug": 9.0, "Eg": 0.123e4, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0},
    19: {"ug": 9.5, "Eg": 749, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0},
    20: {"ug": 10.0, "Eg": 454, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0},
    21: {"ug": 10.5, "Eg": 275, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0},
    22: {"ug": 11.0, "Eg": 167, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0},
    23: {"ug": 11.5, "Eg": 101, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0},
    24: {"ug": 12.0, "Eg": 61.4, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0},
    25: {"ug": 12.5, "Eg": 37.3, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0},
    26: {"ug": 13.0, "Eg": 22.6, "chi1": 0, "chi2": 0, "chi3": 0, "chi456": 0, "chi": 0}
}

## adding another group w/ boundary at 1MeV
g4_chi1 = q3_data[4]["chi1"]
g5_chi1 = q3_data[5]["chi1"]
g4_chi2 = q3_data[4]["chi2"]
g5_chi2 = q3_data[5]["chi2"]
g4_chi3 = q3_data[4]["chi3"]
g5_chi3 = q3_data[5]["chi3"]
g4_chi456 = q3_data[4]["chi456"]
g5_chi456 = q3_data[5]["chi456"]
g4_chi = q3_data[4]["chi"]
g5_chi = q3_data[5]["chi"]

g4_Eg = q3_data[4]["Eg"]
g5_Eg = q3_data[5]["Eg"]

chi_multi = (g4_Eg - 1e6) / (g4_Eg - g5_Eg)

q3_data[4.5] = {
    "Eg": 1e6,
    "chi1": chi_multi * (g4_chi1 - g5_chi1) + g5_chi1,
    "chi2": chi_multi * (g4_chi2 - g5_chi2) + g5_chi2,
    "chi3": chi_multi * (g4_chi3 - g5_chi3) + g5_chi3,
    "chi456": chi_multi * (g4_chi456 - g5_chi456) + g5_chi456,
    "chi": chi_multi * (g4_chi - g5_chi) + g5_chi,
}


q3_tops = np.array([0, 0, 0, 0, 0])
q3_bots = np.array([0, 0, 0, 0, 0])

for group in q3_data:
    match group:
        case 1:
            E_upper = 10e6

            chi1_gp1 = 0
            chi2_gp1 = 0
            chi3_gp1 = 0
            chi456_gp1 = 0
            chi_gp1 = 0
        case 4:
            E_upper = q3_data[4.5]["Eg"]
            chi1_gp1 = q3_data[4.5]["chi1"]
            chi2_gp1 = q3_data[4.5]["chi2"]
            chi3_gp1 = q3_data[4.5]["chi3"]
            chi456_gp1 = q3_data[4.5]["chi456"]
            chi_gp1 = q3_data[4.5]["chi"]
        case 4.5:
            E_upper = q3_data[5]["Eg"]
            chi1_gp1 = q3_data[5]["chi1"]
            chi2_gp1 = q3_data[5]["chi2"]
            chi3_gp1 = q3_data[5]["chi3"]
            chi456_gp1 = q3_data[5]["chi456"]
            chi_gp1 = q3_data[5]["chi"]
        case 26:
            continue
        case _:
            E_upper = q3_data[group + 1]["Eg"]
            chi1_gp1 = q3_data[group + 1]["chi1"]
            chi2_gp1 = q3_data[group + 1]["chi2"]
            chi3_gp1 = q3_data[group + 1]["chi3"]
            chi456_gp1 = q3_data[group + 1]["chi456"]
            chi_gp1 = q3_data[group + 1]["chi"]

    E_lower = q3_data[group]["Eg"]
    deltaE = E_upper - E_lower

    chi1_g = q3_data[group]["chi1"]
    chi2_g = q3_data[group]["chi2"]
    chi3_g = q3_data[group]["chi3"]
    chi456_g = q3_data[group]["chi456"]
    chi_g = q3_data[group]["chi"]

    chi_mult = (deltaE / 2) / (E_upper - E_lower)
    print(group, chi_mult)

    chi1_g_new = chi_mult * (chi1_gp1 - chi1_g) + chi1_g
    chi2_g_new = chi_mult * (chi2_gp1 - chi2_g) + chi2_g
    chi3_g_new = chi_mult * (chi3_gp1 - chi3_g) + chi3_g
    chi456_g_new = chi_mult * (chi456_gp1 - chi456_g) + chi456_g
    chi_g_new = chi_mult * (chi_gp1 - chi_g) + chi_g

    to_add1 = chi1_g_new * deltaE
    to_add2 = chi2_g_new * deltaE
    to_add3 = chi3_g_new * deltaE
    to_add456 = chi456_g_new * deltaE
    to_add = chi_g_new * deltaE

    if E_lower >= 1e6:
        q3_tops[0] += to_add1
        q3_tops[1] += to_add2
        q3_tops[2] += to_add3
        q3_tops[3] += to_add456
        q3_tops[4] += to_add

    q3_bots[0] += to_add1
    q3_bots[1] += to_add2
    q3_bots[2] += to_add3
    q3_bots[3] += to_add456
    q3_bots[4] += to_add


q3_fs = q3_tops / q3_bots

print(q3_fs)
