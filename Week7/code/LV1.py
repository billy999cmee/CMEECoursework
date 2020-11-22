#!/usr/bin/env python3

"""Creating two plots of the Lotka-Volterra model"""

__appname__ = 'LV1.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

# packages
import scipy as sc
import scipy.integrate as integrate
import matplotlib.pylab as p

# Nested function for the utilization of the cProfile module
def wrapping_dcrdt():
    """
    Wrapping dCR_dt for comparing script run time
    using cProfile module
    """
    # The Lotka-Volterra model
    def dCR_dt(pops, t=0):
        """
        This is the classical model of Biology: The Lotka-Volterra model
        for a predator-prey system in 2D space.

            Parameters:
                C: Consumer (e.g. predator)
                R: Resource (e.g. prey)
                r: intrinsic (per-capita) growth rate 
                a: per-capita search rate for R multiplied with its attack success probability
                z: mortality rate
                e: consumer efficiency of converting resource to consumer biomass
            
            Returns:
                A one dimensional array of the model
        """
        R = pops[0]
        C = pops[1]
        dRdt = r * R - a * R * C 
        dCdt = -z * C + e * a * R * C
        
        return sc.array([dRdt, dCdt])

    #assign parameters
    r = 1.0
    a = 0.1 
    z = 1.5
    e = 0.75

    #time vector
    t = sc.linspace(0, 15, 1000)

    #set initial conditions for 2 pop, convert two into an array
    R0 = 10
    C0 = 5 
    RC0 = sc.array([R0, C0])

    pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

    # Plotting
    #open an empty figure object
    f1 = p.figure()
    # Plots
    line1 = p.plot(t, pops[:,0], 'g-', label = 'Resource density') 
    line2 = p.plot(t, pops[:,1]  , 'b-', label = 'Consumer density')
    p.grid()
    p.legend(loc = 'best')
    p.xlabel('Time')
    p.ylabel('Population density')
    p.title('Consumer-Resource population dynamics')
    #p.show() #to display on screen
    f1.savefig('../results/LV_model.pdf') #save figure 

    #second plot
    f2 = p.figure()

    p.plot(pops[:,0], pops[:,1], color = 'red')
    p.grid()
    p.xlabel('Resource density')
    p.ylabel('Consumer density')
    p.title('Consumer-Resource population dynamics')
    #save figure
    f2.savefig('../results/LV_model2.pdf') #Save figure

wrapping_dcrdt()