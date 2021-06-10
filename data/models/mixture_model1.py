#import stuff
from numpy import *
from random import normalvariate, random, uniform
from scipy.optimize import fmin


def get_likelihood( params, subj_trials, res_index, rew_index, n_options):

    alpha = params[0]
    B_value = params[1]
    stay_prob = params[2]
    mixture = params[3]
    neg_log_likelihood = 0.
    
    if (alpha < 0.0) or (alpha > 1.0): return inf
    if B_value < 0.0: return inf
    if (stay_prob < 0.0) or (stay_prob > 0.5): return inf
    if (mixture < 0.0) or (mixture > 1.0): return inf
    
    Q = array([0.0]*n_options)
    
    last_choice = -1
    
    for trial in subj_trials:
        
        probs = [0.25, 0.25, 0.25, 0.25]
        sw_probs = [(1-stay_prob)/(n_options-1), (1-stay_prob)/(n_options-1), (1-stay_prob)/(n_options-1), (1-stay_prob)/(n_options-1)]
        
        numerators = [ exp(i*B_value) for i in Q]
        denominator = sum(numerators)
        
        RL_probs = [ i/denominator for i in numerators]
        
        choice = int(trial[res_index])
        payoff = float(trial[rew_index])
        

            
        sw_probs[choice] = stay_prob

        for i in range(len(probs)):
            probs[i] = (RL_probs[i]*mixture) + (sw_probs[i]*(1-mixture))
            
        if trial[2] == '1':
            prob = 0.25
        else:
        	prob = probs[choice]
        
        neg_log_likelihood += -log( prob )
            
        Q[choice] += alpha * ( payoff - Q[choice] ) 
        last_choice = choice
        
#         print RL_probs
#         print sw_probs
#         print mixture
#         print probs
#         print '*************'
        
    return neg_log_likelihood



def do_fit(subj_data, res_index, rew_index, n_options):
    
    #print subj_data
    
    subj = subj_data[0][0]

    tries = 15
    
    outputs = []
    
    for i in range(tries):

        # sets the starting parameters by picking them randomly
        # params: alpha,  temp
        iparams = [ uniform(0,1),  uniform(0, 3), uniform(0,1), uniform(0,1)]
        #print iparams
        
        res = fmin(get_likelihood, iparams, args=(subj_data, res_index, rew_index, n_options,), full_output=True) #(function to input, parameters to start with, other inputs to the function, output shit)
        
        outputs.append([res[1], res[0]])
        
    minLLH = 1000

    for i in range(len(outputs)):
            if outputs[i][0] < minLLH:
                minLLH = outputs[i][0]
                params = outputs[i][1]

    return [minLLH, params]



		 