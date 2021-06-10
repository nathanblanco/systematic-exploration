#import stuff
from numpy import *
from random import normalvariate, random, uniform
from scipy.optimize import fmin


def get_likelihood( params, subj_trials, res_index, rew_index, n_options):
		
    clockwise_prob = params[0] # prob of choosing the option clockwise from the last choice
    counterclockwise_prob = params[1] # prob of moving on counter clockwise
    skip_prob = params[2] # prob of skipping across (which is also 2 ahead or 2 back)
    stay_prob = 1 - clockwise_prob - counterclockwise_prob - skip_prob
    neg_log_likelihood = 0.

    
    if ( (clockwise_prob + counterclockwise_prob + skip_prob) > 1.0): return inf
    if ( clockwise_prob  < 0.0): return inf
    if ( counterclockwise_prob  < 0.0): return inf
    if ( skip_prob  < 0.0): return inf
    
    # invalid if staying or skipping is more than cw or ccw
#     if (stay_prob > clockwise_prob) & (stay_prob > counterclockwise_prob): return inf
#     if (skip_prob > clockwise_prob) & (skip_prob > counterclockwise_prob): return inf

    # one of these has to be high for it to truly be a pattern model
    if (clockwise_prob < 0.5) & (counterclockwise_prob < 0.5): return inf

    
    last_choice = -1
    

    order = [2, 3, 1, 0]

        
    for trial in subj_trials:
    
        choice = int(trial[res_index])
        
        if trial[2] == '1':
            prob = 0.25
            
        elif int(choice) == order[ (order.index(int(last_choice))+1)%len(order)]:
            prob = clockwise_prob
        elif int(choice) == order[ (order.index(int(last_choice))-1)%len(order)]:
            prob = counterclockwise_prob
        elif int(choice) == order[ (order.index(int(last_choice))+2)%len(order)]:
            prob = skip_prob
        else:
            prob = stay_prob
            
#         print ('*****')
# #         print direction
#         print (choice)
#         print (last_choice)
#         print (params)
#         print (prob)
#         print ('*****')
            
        last_choice = choice
        
        neg_log_likelihood += -log( prob )
        
    return neg_log_likelihood



def do_fit(subj_data, res_index, rew_index, n_options):
    
    #print subj_data
    
    subj = subj_data[0][0]

    tries = 50
    
    outputs = []
    
    for i in range(tries):

        # sets the starting parameters by picking them randomly
        # params: alpha,  temp
        iparams = [ uniform(0,0.8), uniform(0,0.8), uniform(0,0.1)]
        #print iparams
        
        res = fmin(get_likelihood, iparams, args=(subj_data, res_index, rew_index, n_options), full_output=True) #(function to input, parameters to start with, other inputs to the function, output shit)
        
        outputs.append([res[1], res[0]])
        
    minLLH = 1000

    for i in range(len(outputs)):
            if outputs[i][0] < minLLH:
                minLLH = outputs[i][0]
                params = outputs[i][1]

#    print params
    return [minLLH, params[0], params[1], params[2]]



		 