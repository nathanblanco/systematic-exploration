import os, sys, signal
#from openfile import openfile
import clockwise_model
import exploration_bonus_mixture
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


f = open("modeling results/aics_adults_rotate.txt", "w+")

f.write('subj Lag CW CCW \n')

n_trials = 100

beh_data = openfile('BL_NV_combined_adults.txt', n_trials)

#Fits = []
#files = os.listdir(dir)

def get_aic(nllh, n_params):
    aic = 2*nllh + 2*n_params
    return aic

for subj_data in beh_data[:]:
        
        subj = subj_data[0][0]
        
        [ebm_nllh, ebm_Bev, ebm_weight] = exploration_bonus_mixture.do_fit(subj_data, 4, 6, 4)
        cw_nllh = clockwise_model.do_fit(subj_data, 4, 6, 4, 'clockwise')
        ccw_nllh = clockwise_model.do_fit(subj_data, 4, 6, 4, 'counterclockwise')
        
        outputs =  [get_aic(ebm_nllh, 3), get_aic(cw_nllh, 1), get_aic(ccw_nllh, 1)]
        
        f.write(subj+' ')
        for i in outputs:
            f.write(str(i)+' ')
        f.write('\n')
        f.flush()


