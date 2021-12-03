import numpy as np

test = open('day3.txt', 'r')
input = test.read()

bits = np.array(
    [list(map(int, list(line))) for line in input.splitlines()])
def rating(type, bits):
    m, n = bits.shape
    i, indicies = 0, np.array([True]*m)
    while i < n and np.count_nonzero(indicies) > 1:
        col = bits[:,i]
        m = np.mean(col[indicies])
        if type == 'g':
            most_common = 1 if m >= 0.5 else 0
        else:
            most_common = 0 if m >= 0.5 else 1
        indicies *= (col == most_common)
        i += 1
    remaining = bits[np.argwhere(indicies)[0,0],:]
    return int(''.join(map(str, remaining)), 2)

print(rating('g', bits) * rating('s', bits))