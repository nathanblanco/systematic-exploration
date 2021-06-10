#import stuff
from numpy import *
from random import normalvariate, random, uniform, shuffle, randint
from scipy.optimize import fmin


def openfile(filename):
    myfile = open(filename, 'rU')
    alldata = myfile.readlines()
    
    for i in range(len(alldata)):
        alldata[i]=alldata[i].split()
        
    datas = []
    for line in alldata:

        datas.append(line)
      
    return datas


def simulate_pattern(subj, clockwise_prob, counterclockwise_prob, skip_prob, n_trials, n_sims):


    stay_prob = 1 - clockwise_prob - counterclockwise_prob - skip_prob
    
    order = [2, 3, 1, 0]
    
    outputs = []
    rewards = [10, 3, 2, 1]

    for sim in range(n_sims):
    
        shuffle(rewards)
        prev_res = 0
        lag = array([1]*4)
        
        
        
        for trial in range(n_trials):
        
            door_loc = randint(0,3)

            choice_rand = random()
        
            if trial == 1:
                probs = [0.25, 0.25, 0.25, 0.25]
            
            else:
                probs = [0, 0, 0, 0]
                
                probs[  order[ (order.index(int(prev_res))+1)%len(order)]  ] = clockwise_prob
                probs[  order[ (order.index(int(prev_res))-1)%len(order)]  ] = counterclockwise_prob
                probs[  order[ (order.index(int(prev_res))+2)%len(order)]  ] = skip_prob
                
                for i in range(len(probs)):
                    if probs[i] == 0:
                        probs[i] = stay_prob
                        
                print ('***************')
                print(probs)
                            
                
            if choice_rand <= probs[0]:
                res = 0
            elif choice_rand <= probs[0]+probs[1]:
                res = 1
            elif choice_rand <= probs[0]+probs[1]+probs[2]:
                res = 2
            else:
                res = 3
                
            payoff = rewards[res]
            
            chose_door = int(res==door_loc)
            
            stay = int(prev_res == res)
    
            outputs.append([subj, 'Pattern', clockwise_prob, counterclockwise_prob, skip_prob, sim, (subj*1000)+sim, trial, res, payoff, chose_door, stay, lag[res], rewards[prev_res]])
            
            lag = [i+1 for i in lag]
            lag[res] = 1
            
            print (choice_rand)
            print (lag)
    
            prev_res = res
            
    return outputs




def do_sims(datafile, outputfile, n_trials, n_sims):

    subj_data = openfile(datafile)
    
    f = open(outputfile, 'w+')
    
    for line in subj_data[1:]:
        subj =  int(line[0])
        cw = float(line[2])
        ccw = float(line[3])
        skip = float(line[4])

        
        pattern_outputs = simulate_pattern(subj, cw, ccw, skip, n_trials, n_sims)
        
        for line in pattern_outputs:
            for thing in line:
                f.write( str(thing)+'\t')
            f.write('\n')
            f.flush()

            
#--------------------------------------------------------------#

n_trials = 100
n_sims = 100


datafile = '/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data/models/modeling results/pattern_model_best_children.txt'
outputfile = '/Users/nathanblanco/Dropbox/Work/current_projects/exploration studies/Baseline/data/models/modeling results/pattern_sims.txt'

do_sims(datafile, outputfile, n_trials, n_sims)



