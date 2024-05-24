import h5py
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MultipleLocator, AutoMinorLocator

# Nome del file HDF5
file_name = 'curves.h5'

# Leggere il file HDF5
with h5py.File(file_name, 'r') as hdf:
    curves = []
    for i in range(1, len(hdf['curves'])+1):  # 5 curve
        dataset_name = f'curves/curve_{i}'
        dataset = hdf[dataset_name]
        # Estrarre i dati x e y e trasporli se necessario
        data = dataset[:]
        if data.shape[0] == 2:  # Caso (2, 100)
            x = data[0, :]
            y = data[1, :]
        else:  # Caso (100, 2)
            x = data[:, 0]
            y = data[:, 1]
        curves.append((x, y))

# Creare il plot
plt.figure()

for i, (x, y) in enumerate(curves, start=1):
    plt.plot(x, y, label=f'Curve {i}')

# Abilita i tick minori interni
plt.gca().xaxis.set_minor_locator(AutoMinorLocator())
plt.gca().yaxis.set_minor_locator(AutoMinorLocator())

# Imposta gli intervalli degli assi
plt.gca().xaxis.set_major_locator(MultipleLocator(10))
plt.gca().yaxis.set_major_locator(MultipleLocator(10))

plt.xlabel('X', fontsize=30, labelpad=20)
plt.ylabel('Y', fontsize=30, labelpad=20)
# plt.title('Five Curves', fontsize=30)
# plt.legend(fontsize=30, loc='upper left', frameon=False, fancybox=True, shadow=True, borderpad=1, ncol=5)

# Imposta lo stile dei tick minori e maggiori
plt.tick_params(axis='both', which='both', direction='in', width=3, length=10, labelsize=30, pad=20)

# Imposta lo spessore del frame del plot
for spine in plt.gca().spines.values():
    spine.set_linewidth(3)

# Imposta i limiti degli assi per partire dal valore minimo dei dati
x_min = min([x.min() for x, y in curves])
y_min = min([y.min() for x, y in curves])
if x_min >= 0:
    plt.xlim(x_min, None)
if y_min >= 0:
    plt.ylim(0, None)

# Calcola i limiti degli assi come multipli del passo degli intervalli degli assi
x_min = min([x.min() for x, y in curves])
x_max = max([x.max() for x, y in curves])
y_min = min([y.min() for x, y in curves])
y_max = max([y.max() for x, y in curves])

x_step = 10
y_step = 10

x_min = x_step * np.floor(x_min / x_step)
x_max = x_step * np.ceil(x_max / x_step)
y_min = y_step * np.floor(y_min / y_step)
y_max = y_step * np.ceil(y_max / y_step)

plt.xlim(x_min, x_max)
plt.ylim(y_min, y_max)

# Rimuovi la griglia
plt.grid(False)

plt.show()

