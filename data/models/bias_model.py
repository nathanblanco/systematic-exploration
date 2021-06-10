#import stuff
from numpy import *
from random import normalvariate, random, uniform
from scipy.optimize import fmin


def get_likelihood( params, subj_trials, res_index, rew_index, n_options):
		
    probA = params[0]
    probB = params[1]
    probC = params[2]
    neg_log_likelihood = 0.

    
    if (probA < 0.0) or (probA > 1.0): return inf
    if (probB < 0.0) or (probB > 1.0): return inf
    if (probC < 0.0) or (probC > 1.0): return inf
    if (probA+probB+probC >= 1.0): return inf

    
    for trial in subj_trials:
    
        choice = int(trial[res_index])
        
        if choice == 0:
            prob = probA
        elif choice == 1:
            prob = probB
        elif choice == 2:
            prob = probC
        else: 
            prob = 1 - probA - probB - probC
        
        neg_log_likelihood += -log( prob )
        
    return neg_log_likelihood



def do_fit(subj_data, res_index, rew_index, n_options):
    
    #print subj_data
    
    subj = subj_data[0][0]

    tries = 15
    
    outputs = []
    
    for i in range(tries):

        # sets the starting parameters by picking them randomly
        # params: alpha,  temp
        iparams = [ uniform(0,.3), uniform(0,.3), uniform(0,.3) ]
        #print iparams
        
        res = fmin(get_likelihood, iparams, args=(subj_data, res_index, rew_index, n_options,), full_output=True) #(function to input, parameters to start with, other inputs to the function, output shit)
        
        outputs.append([res[1], res[0]])
        
    minLLH = 1000

    for i in range(len(outputs)):
            if outputs[i][0] < minLLH:
                minLLH = outputs[i][0]
                params = outputs[i][1]

    return minLLH



		 