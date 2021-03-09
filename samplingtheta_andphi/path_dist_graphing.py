import numpy as np
import matplotlib.pyplot as plt

data= np.loadtxt('path_graph.txt')

x1=data[:,1]
y1=data[:,0]

plt.plot(x1,y1, label='path length')
plt.yscale('log')
plt.legend()
plt.show()
#plt.savefig('path_dist.pdf')
#plt.close()