import numpy as np
import scipy as sci
from scipy import optimize
import matplotlib.pyplot as plt

def tanf(x,tau):
    
    return np.tan(x) - x/(1.-1.5*tau)

def prob_ct_tau(path,tau,mfp=1.):
    """
    Compute (derivative) of cumulative probability for given optical depth
    and mean free path
    """
    
    # Compute dimensionless path length t
    path0 = mfp*(tau+2./3)**2*3./np.pi**2
    t = path/path0

    # Compute P(t) as sum over series
    y = np.exp(-t)
    prob = 0.
  
    n = 1
    sol = sci.optimize.root_scalar(tanf,args=(tau),bracket=[0.51*np.pi,1.49*np.pi])
    lamn = sol.root
    dlamn = lamn
    In = 0.5*tau*(1+(1.5*tau-1)/((1.5*tau-1)**2+lamn*lamn))
    yn2 = (y)**(lamn*lamn/(np.pi*np.pi))*np.cos(lamn)*(1.5*tau**2/(1-1.5*tau)/In)*(lamn*lamn/(np.pi*np.pi))

    # Compute the sum to the limit of double precision
    while (abs(yn2) > 1.e-17 ): 
        prob = prob + yn2
        n = n+1;
        #sol=sci.optimize.root_scalar(tanf,args=(tau),x0=np.pi*n,x1=(np.pi+0.1)*n)
        bracket=[lamn+(1.-0.1/n)*dlamn,lamn+(1.+0.1/n)*dlamn]
        sol=sci.optimize.root_scalar(tanf,args=(tau),bracket=bracket)
        dlamn = sol.root-lamn
        lamn = sol.root
        In = 0.5*tau*(1+(1.5*tau-1)/((1.5*tau-1)**2+lamn*lamn))
        yn2 = (y)**(lamn*lamn/(np.pi*np.pi))*np.cos(lamn)*(1.5*tau**2/(1-1.5*tau)/In)*(lamn*lamn/(np.pi*np.pi))

    # Return probability normalized by path0

    return prob/path0

tau = 40
mfp=1.
n=100
pmin=1.
pmax=40*mfp*(tau+2./3)**2*3./np.pi**2
print(pmax /40.0)
dp = np.log10(pmax/pmin)/float(n)
path = 10**(np.arange(np.log10(pmin),np.log10(pmax),dp))

prob = np.zeros(n)
for i in range(n):
    prob[i] = prob_ct_tau(path[i],tau,mfp)

plt.plot(path,prob, label='analytic')
plt.yscale('log')

data= np.loadtxt('path_graph.txt')

x1=data[:,1]
y1=data[:,0]

plt.plot(x1,y1, label='path length')
plt.yscale('log')
plt.legend()
plt.show()
#plt.savefig('path_comparison2.pdf')
#plt.close()
