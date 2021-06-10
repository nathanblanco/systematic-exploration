#python27

import os, sys, signal
#from openfile import openfile
# import switch_model
# import RL_auto
# import RL_Lag
import markov_pattern_model
# import bias_model
# import mixture_model1
#import exploration_bonus

#import exploration_bonus_mixture
from numpy import *


def openfile(filename, n_trials):
    myfile = open(filename)
    alldata = myfile.readlines()
    
    for i in range(len(alldata)):
        alldata[i]=alldata[i].split()
        
    allsubjdata = []
    subj = 'x'
    subjindex = -1
    for line in alldata:
        if int(line[2]) <= n_trials:
            if (line[0] != subj):
                subj = line[0]
                allsubjdata.append([line])
                subjindex += 1
            else:
                allsubjdata[subjindex].append(line)
    
    #print allsubjdata
    return allsubjdata


#f = open("adults_baseline_aics.txt", "w+")
#f = open("child_baseline_aics.txt", "w+")
#f = open("adults_novelty1_aics.txt", "w+")
#f = open("child_novelty10_aics.txt", "w+")

f = open("modeling results/pattern_model_adults.txt", "w+")
#f = open("modeling results/pattern_model_children.txt", "w+")

f.write('subj Pattern_aic cw ccw skip\n')

n_trials = 100

#beh_data = openfile('baseline_children.txt', n_trials)


beh_data = openfile('BL_NV_combined_adults.txt', n_trials)

#Fits = []
#files = os.listdir(dir)

def get_aic(nllh, n_params):
    aic = 2*nllh + 2*n_params
    return aic


for subj_data in beh_data[:]:
        
        subj = subj_data[0][0]
        
        [nllh, cw, ccw, skip] = markov_pattern_model.do_fit(subj_data, 4, 6, 4)
        
        outputs =  [get_aic(nllh, 3), cw, ccw, skip]
        
        f.write(subj+' ')
        for i in outputs:
            f.write(str(i)+' ')
        f.write('\n')
        f.flush()


