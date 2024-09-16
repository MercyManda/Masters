import numpy as np
from collections import defaultdict

DATA_PATH = "original/data/data.txt"



def get_data():
    data_values = defaultdict(lambda : np.array([]))
    with open(DATA_PATH, "r") as file:
        data = file.readlines()

    for line in range(1, len(data)):
        item = data[line].strip().split()
        key = int(item[0].strip('"'))
        values = np.array([float(i) for i in item[1:]])
        data_values[key] = np.concatenate((data_values[key], values))
    return data_values
    


if __name__ == "__main__":
    v = get_data()
    print(type(v[1]))
