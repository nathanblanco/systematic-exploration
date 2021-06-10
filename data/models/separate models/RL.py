#import stuff
from numpy import *
from random import normalvariate, random, uniform
from scipy.optimize import fmin


def get_likelihood( params, subj_trials, res_index, rew_index, n_options):
		
    alpha = 1.0 # fixed alpha at 1
    B_value = params[0]
    neg_log_likelihood = 0.
    
    if (alpha < 0.0) or (alpha > 1.0): return inf
    if B_value < 0.0: return inf
    
    Q = array([0.0]*n_options)
    
    #('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')

    
    for trial in subj_trials:
        
        
        numerators = [ exp(i*B_value) for i in Q]
        denominator = sum(numerators)
        
        probs = [ i/denominator for i in numerators]
        
        res = int(trial[res_index])
        payoff = float(trial[rew_index])
        
        prob = probs[res]
        
        neg_log_likelihood += -log( prob )
            
        Q[res] += alpha * ( payoff - Q[res] ) 
        
    return neg_log_likelihood


# athataltlkajslkltjastlkajstkastdj  

def do_fit(subj_data, res_index, rew_index, n_options):
    
    #print subj_data
    
    subj = subj_data[0][0]

    tries = 15
    
    outputs = []
    
    for i in range(tries):

        # sets the starting parameters by picking them randomly
        # params: alpha,  temp
        iparams = [ uniform(0, 3)]
        #print iparams
        
        res = fmin(get_likelihood, iparams, args=(subj_data, res_index, rew_index, n_options,), full_output=True) #(function to input, parameters to start with, other inputs to the function, output shit)
        
        outputs.append([res[1], res[0]])
        
    minLLH = 1000

    for i in range(len(outputs)):
            if outputs[i][0] < minLLH:
                minLLH = outputs[i][0]
                params = outputs[i][1]

    return [minLLH, params[0]]



		 