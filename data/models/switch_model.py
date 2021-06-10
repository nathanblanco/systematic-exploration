#import stuff
from numpy import *
from random import normalvariate, random, uniform
from scipy.optimize import fmin


def get_likelihood( params, subj_trials, res_index, rew_index, n_options):
		
    stay_prob = params[0]
    neg_log_likelihood = 0.

    
    if (stay_prob < 0.0) or (stay_prob > 0.5): return inf
    
    last_choice = -1
    
    for trial in subj_trials:
    
        choice = int(trial[res_index])
        
        if trial[2] == '1':
            prob = 0.25
            
        elif choice == last_choice:
            prob = stay_prob
        else:
            prob = (1-stay_prob)/(n_options-1)
            
        #print choice
        #print last_choice
        #print stay_prob
        #print prob
        #print '*****'
            
        last_choice = choice
        
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
        iparams = [ uniform(0,1)]
        #print iparams
        
        res = fmin(get_likelihood, iparams, args=(subj_data, res_index, rew_index, n_options,), full_output=True) #(function to input, parameters to start with, other inputs to the function, output shit)
        
        outputs.append([res[1], res[0]])
        
    minLLH = 1000

    for i in range(len(outputs)):
            if outputs[i][0] < minLLH:
                minLLH = outputs[i][0]
                params = outputs[i][1]

    print params
    return [minLLH, params[0]]



		 