import numpy as np
import matplotlib.pyplot as plt

dat= np.loadtxt('limb_dark.txt')

x1=dat[:,0]
y1=dat[:,1]

plt.scatter(x1,y1,s=.05)
plt.show()
