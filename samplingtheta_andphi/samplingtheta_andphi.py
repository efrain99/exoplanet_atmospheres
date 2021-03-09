import numpy as np
import matplotlib.pyplot as plt


#data= np.loadtxt('phi_output.txt')
data2= np.loadtxt('theta_output.txt')

#x1=data[:,0]
#y1=data[:,1]
#x2=data[:,2]

x2=data2[:,0]
y3=data2[:,1]
y4= np.sin(x2)/2.0
plt.plot(x2,y3, label='thetac')
plt.plot(x2,y4)
#plt.plot(x1,y2)
plt.xlabel('theta')
plt.ylabel('distibution')
plt.legend()
plt.show()
#plt.savefig('analytic_theta .pdf')


