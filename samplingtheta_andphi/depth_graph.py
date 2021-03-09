import numpy as np
import matplotlib.pyplot as plt


dat= np.loadtxt('graphing.txt')

#x1=dat[:,1]
#y1=dat[:,0]
x2=dat[:,3]
y2=dat[:,2]
y3= 1.0/(np.pi*2.0)
#y4= np.sin(x1)/2.0

plt.plot(x2,y2, label='phic')
#plt.plot(x1,y4)
plt.plot(y3)
plt.xlabel('phic')
plt.ylabel('dist_phi')
plt.legend()
plt.show()
#plt.savefig('phi_dist.pdf')
