from numpy import *
from scipy.stats import *
from string import split
from random import random, choice, uniform
from scipy.optimize import fmin


def getModelLikelihood(params, subj_trials, res_index, rew_index, n_options):

    alpha = params[0]
    Bvalue = params[1]
    Blag = params[2]
    neg_log_likelihood = 0.
    
    if (alpha < 0.0) or (alpha > 1.0): return inf
    if (Bvalue < 0.0): return inf
    if (Blag < 0.0): return inf
    
    Q = array([0.0]*n_options)
    
    lag = array([1]*n_options)
    
    prev_res = 0

    for trial in subj_trials:
    
        res = int(trial[res_index])
        payoff = float(trial[rew_index])
        
        #print prev_res
        #print 'Response \n'
        #print res
        #print 'Lag array \n'
        #print lag
        #print '******'

        numerators = [ exp(Bvalue*Q[i] + Blag*lag[i]) for i in range(len(Q))]
        denominator = sum(numerators)
        probs = [ i/denominator for i in numerators]
        #print probs
        
        prob = probs[res]

        neg_log_likelihood += -log( prob )

        Q[res] += alpha * (payoff - Q[res])
        
        lag = [i+1 for i in lag]
        lag[res] = 1
        
        prev_res = res
        
    return neg_log_likelihood


def do_fit(subj_data, res_index, rew_index, n_options):
    
    
    subj = subj_data[0][0]
    
    tries = 15
    
    outputs = []
        
    for i in range(tries):
      
        # sets the starting parameters by picking them randomly
        # params: alpha, exploit
        iparams = [ uniform(0,1), uniform(0, 1), uniform(0,1)]
        
        res = fmin(getModelLikelihood, iparams, args=(subj_data, res_index, rew_index, n_options,), full_output=True) #(function to input, parameters to start with, other inputs to the function, output shit)

        outputs.append([res[1], res[0]])
        
    minLLH = 1000

    for i in range(len(outputs)):
            if outputs[i][0] < minLLH:
                minLLH = outputs[i][0]
                params = outputs[i][1]
        
    return [minLLH, params[1], params[2]]


    


