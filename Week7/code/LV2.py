#!/usr/bin/env python3

"""Lotka-Volterra model for a predator-prey system in 2D space"""

__appname__ = 'LV2.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

# packages
import sys
import scipy as sc
import scipy.integrate as integrate
import matplotlib.pylab as p
import matplotlib.patches as mpatches

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
                K: the carrying capacity of the prey
            
            Returns:
                A one dimensional array of the model
        """
        R = pops[0]
        C = pops[1]
        dRdt = r * R * (1-(R/K)) - a * R * C 
        dCdt = -z * C + e * a * R * C
        
        return sc.array([dRdt, dCdt])

    # Arguments for the passing into command line
    # Skip argv[0] as it is the program's location
    # check whether there are input arguments, if not use default parameters
    if len(sys.argv) <5:
        print("There are no input for model parameters, we will use defaults values")
        r = 1
        a = 0.15
        z = 1.5
        e = 0.75
    else:
        print("Using the input parameters for the LV model")
        r = float(sys.argv[1]) 
        a = float(sys.argv[2]) 
        z = float(sys.argv[3]) 
        e = float(sys.argv[4]) 

    # assign parameters
    K = 37

    # time vector
    t = sc.linspace(0, 15, 1000)

    #set initial conditions for 2 pop, convert two into an array
    R0 = 10
    C0 = 5 
    RC0 = sc.array([R0, C0])

    pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

    # Plotting
    #open an empty figure object
    f1 = p.figure()
    #plots
    p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
    p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
    p.grid()
    # First legend
    p.legend(loc='best')

    p.title("r = %.2f, a = %.2f, z = %.2f, e = %.2f" %(r, a, z, e), fontsize = 8)
    p.suptitle('Consumer-Resource population dynamics')
    p.xlabel('Time')
    p.ylabel('Population density')

    #p.show() # display on screen
    #save figure
    f1.savefig('../results/LV_model_preyden.pdf') 


    print('The final population size of consumers is:', int(pops[(pops.shape[0]-1),1]), 'individuals') #for a matrix of shape(n,m) where n=rows and m=columns, shape[0] gives the rows
    print('The final population size of resources is:', int(pops[(pops.shape[0]-1),0]), 'individuals')

wrapping_dcrdt()

# Add parameters to label, having 2 legends!
#red_patch = mpatches.Patch(color = 'red', label =  r)
#yellow_patch = mpatches.Patch(color = 'yellow', label = a)
#orange_patch = mpatches.Patch(color = 'orange', label = z)
#purple_patch = mpatches.Patch(color = 'purple', label = z)
# Second legend, will remove the first legend
#p.legend(handles = [yellow_patch, red_patch, orange_patch, purple_patch], loc = 'lower right')
# Add first legend back
#p.gca().add_artist(first_legend)