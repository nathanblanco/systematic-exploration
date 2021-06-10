#import stuff
from numpy import *
from random import normalvariate, random, uniform
from scipy.optimize import fmin


def get_likelihood( params, subj_trials, res_index, rew_index, n_options, direction):
		
    rotate_prob = params[0]
    neg_log_likelihood = 0.

    
    if (rotate_prob < 0.5) or (rotate_prob > 1.0): return inf
    
    last_choice = -1
    
    if direction == 'clockwise':
        order = [2, 3, 1, 0]
    elif direction == 'counterclockwise':
        order = [0, 1, 3, 2]
        
    for trial in subj_trials:
    
        choice = int(trial[res_index])
        
        if trial[2] == '1':
            prob = 0.25
            
        elif int(choice) == order[ (order.index(int(last_choice))+1)%len(order)]:
            prob = rotate_prob
        else:
            prob = (1-rotate_prob)/(n_options-1)
            
#         print '*****'
#         print direction
#         print choice
#         print last_choice
#         print rotate_prob
#         print prob
#         print '*****'
            
        last_choice = choice
        
        neg_log_likelihood += -log( prob )
        
    return neg_log_likelihood



def do_fit(subj_data, res_index, rew_index, n_options, direction):
    
    #print subj_data
    
    subj = subj_data[0][0]

    tries = 15
    
    outputs = []
    
    for i in range(tries):

        # sets the starting parameters by picking them randomly
        # params: alpha,  temp
        iparams = [ uniform(0,1)]
        #print iparams
        
        res = fmin(get_likelihood, iparams, args=(subj_data, res_index, rew_index, n_options, direction), full_output=True) #(function to input, parameters to start with, other inputs to the function, output shit)
        
        outputs.append([res[1], res[0]])
        
    minLLH = 1000

    for i in range(len(outputs)):
            if outputs[i][0] < minLLH:
                minLLH = outputs[i][0]
                params = outputs[i][1]

    print params
    return minLLH



		 