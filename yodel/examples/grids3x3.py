import torch
import pyro
import pyro.distributions as dist
from pyro.infer import Importance, EmpiricalMarginal

# def ising_model():
#     def flip(n, p):
#         return pyro.sample("flip"+n, dist.Bernoulli(p))

#     x00 = flip("00", 1.0 / 2.0)
#     x01 = flip("011", 1.0 / 3.0) if x00 == 1 else flip("012", 1.0 / 4.0)
#     x02 = flip("021", 1.0 / 3.0) if x01 == 1 else flip("022", 1.0 / 4.0)
#     x10 = flip("101", 1.0 / 5.0) if x00 == 0 else flip("102", 1.0 / 6.0)
#     x20 = flip("201", 1.0 / 5.0) if x10 == 0 else flip("202", 1.0 / 6.0)

#     if x10 == 1 and x01 == 1:
#         x11 = flip("111", 1.0 / 7.0)
#     elif x10 == 1:
#         x11 = flip("112", 1.0 / 8.0)
#     elif x01 == 1:
#         x11 = flip("113", 1.0 / 9.0)
#     else:
#         x11 = flip("114", 1.0 / 11.0)

#     if x20 == 1 and x11 == 1:
#         x21 = flip("211", 2.0 / 7.0)
#     elif x20 == 1:
#         x21 = flip("212", 2.0 / 8.0)
#     elif x11 == 1:
#         x21 = flip("213", 2.0 / 9.0)
#     else:
#         x21 = flip("214", 2.0 / 11.0)

#     if x11 == 1 and x02 == 1:
#         x12 = flip("121", 6.0 / 7.0)
#     elif x11 == 1:
#         x12 = flip("122", 6.0 / 8.0)
#     elif x02 == 1:
#         x12 = flip("123", 6.0 / 9.0)
#     else:
#         x12 = flip("124", 6.0 / 11.0)

#     if x21 == 1 and x12 == 1:
#         x22 = flip("221", 3.0 / 7.0)
#     elif x21 == 1:
#         x22 = flip("222", 3.0 / 8.0)
#     elif x12 == 1:
#         x22 = flip("223", 8.0 / 9.0)
#     else:
#         x22 = flip("224", 9.0 / 11.0)

#     return (x00, x01, x10, x02, x20, x11, x12, x21, x22)


def ising_model():
    def flip(n, p):
        return pyro.sample("flip"+n, dist.Bernoulli(p))

    x00 = flip("00", 1.0 / 2.0)
    x01 = flip("01", 1.0 / 3.0) if x00 == 1 else flip("01", 1.0 / 4.0)
    x02 = flip("02", 1.0 / 3.0) if x01 == 1 else flip("02", 1.0 / 4.0)
    x10 = flip("10", 1.0 / 5.0) if x00 == 0 else flip("10", 1.0 / 6.0)
    x20 = flip("20", 1.0 / 5.0) if x10 == 0 else flip("20", 1.0 / 6.0)

    if x10 == 1 and x01 == 1:
        x11 = flip("11", 1.0 / 7.0)
    elif x10 == 1:
        x11 = flip("11", 1.0 / 8.0)
    elif x01 == 1:
        x11 = flip("11", 1.0 / 9.0)
    else:
        x11 = flip("11", 1.0 / 11.0)

    if x20 == 1 and x1 == 1:
        x21 = flip("21", 2.0 / 7.0)
    elif x20 == 1:
        x21 = flip("21", 2.0 / 8.0)
    elif x11 == 1:
        x21 = flip("21", 2.0 / 9.0)
    else:
        x21 = flip("21", 2.0 / 11.0)

    if x11 == 1 and x0 == 1:
        x12 = flip("12", 6.0 / 7.0)
    elif x11 == 1:
        x12 = flip("12", 6.0 / 8.0)
    elif x02 == 1:
        x12 = flip("12", 6.0 / 9.0)
    else:
        x12 = flip("12", 6.0 / 11.0)

    if x21 == 1 and x1 == 1:
        x22 = flip("22", 3.0 / 7.0)
    elif x21 == 1:
        x22 = flip("22", 3.0 / 8.0)
    elif x12 == 1:
        x22 = flip("22", 8.0 / 9.0)
    else:
        x22 = flip("22", 9.0 / 11.0)

    return (x00, x01, x10, x02, x20, x11, x12, x21, x22)

# print(ising_model())

num_samples = 10000

importance = Importance(ising_model, num_samples=num_samples)
posterior = importance.run()

# # Extract marginal distribution for each variable
# x00_marg   = EmpiricalMarginal(posterior, "flip00")
# x01_marg_1 = EmpiricalMarginal(posterior, "flip011")
# x01_marg_2 = EmpiricalMarginal(posterior, "flip012")

# x02_marg_1 = EmpiricalMarginal(posterior, "flip021")
# x02_marg_2 = EmpiricalMarginal(posterior, "flip022")

# x10_marg_1 = EmpiricalMarginal(posterior, "flip101")
# x10_marg_2 = EmpiricalMarginal(posterior, "flip102")

# x20_marg_1 = EmpiricalMarginal(posterior, "flip201")
# x20_marg_2 = EmpiricalMarginal(posterior, "flip202")

# x11_marg_1 = EmpiricalMarginal(posterior, "flip111")
# x11_marg_2 = EmpiricalMarginal(posterior, "flip112")
# x11_marg_3 = EmpiricalMarginal(posterior, "flip113")
# x11_marg_4 = EmpiricalMarginal(posterior, "flip114")

# x20_marg_1 = EmpiricalMarginal(posterior, "flip201")
# x20_marg_2 = EmpiricalMarginal(posterior, "flip202")
# x20_marg_3 = EmpiricalMarginal(posterior, "flip203")
# x20_marg_4 = EmpiricalMarginal(posterior, "flip204")

# x21_marg_1 = EmpiricalMarginal(posterior, "flip211")
# x21_marg_2 = EmpiricalMarginal(posterior, "flip212")
# x21_marg_3 = EmpiricalMarginal(posterior, "flip213")
# x21_marg_4 = EmpiricalMarginal(posterior, "flip214")

# x12_marg_1 = EmpiricalMarginal(posterior, "flip121")
# x12_marg_2 = EmpiricalMarginal(posterior, "flip122")
# x12_marg_3 = EmpiricalMarginal(posterior, "flip123")
# x12_marg_4 = EmpiricalMarginal(posterior, "flip124")

# x22_marg_1 = EmpiricalMarginal(posterior, "flip221")
# x22_marg_2 = EmpiricalMarginal(posterior, "flip222")
# x22_marg_3 = EmpiricalMarginal(posterior, "flip223")
# x22_marg_4 = EmpiricalMarginal(posterior, "flip224")

# print("Expectation of x00:", x00_marg.item())
# print("Expectation of x01:", x01_marg_1.item(), x01_marg_2.item())
# print("Expectation of x02:", x02_marg_1.item(), x02_marg_2.item())
# print("Expectation of x10:", x10_marg_1.item(), x10_marg_2.item())
# print("Expectation of x20:", x20_marg_1.item(), x20_marg_2.item())
# print("Expectation of x11:", x11_marg_1.item(), x11_marg_2.item(), x11_marg_3.item(), x11_marg_4.item())
# print("Expectation of x20:", x20_marg_1.item(), x20_marg_2.item(), x20_marg_3.item(), x20_marg_4.item())
# print("Expectation of x21:", x21_marg_1.item(), x21_marg_2.item(), x21_marg_3.item(), x21_marg_4.item())
# print("Expectation of x12:", x12_marg_1.item(), x12_marg_2.item(), x12_marg_3.item(), x12_marg_4.item())
# print("Expectation of x22:", x22_marg_1.item(), x22_marg_2.item(), x22_marg_3.item(), x22_marg_4.item())
