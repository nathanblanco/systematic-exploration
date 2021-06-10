import os, sys, signal
import RL_Lag
import RL
import bias_model
import RL_Lag_door
import RL_door

from numpy import *


def openfile(filename, n_trials):
    myfile = open(filename, 'rU')
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

f = open("modeling results/door_fits.txt", "w+")

f.write('subj bias_aic RL_aic RL_Lag_aic RL_door_aic RL_Lag_door_aic Bev Blag Bdoor \n')



n_trials = 100


beh_data = openfile('door_data.txt', n_trials)


def get_aic(nllh, n_params):
    aic = 2*nllh + 2*n_params
    return aic


for subj_data in beh_data[:]:
        
        subj = subj_data[0][0]
            
        
        bias_nllh = bias_model.do_fit(subj_data, 4, 6, 4)    
        rl_nllh = RL.do_fit(subj_data, 4, 6, 4)

        [lag_nllh, lBev, lBlag] = RL_Lag.do_fit(subj_data, 4, 6, 4)
        [door_nllh, dBev, dBdoor] = RL_door.do_fit(subj_data, 4, 6, 9, 4)
        [lag_door_nllh, Bev, Blag, Bdoor] = RL_Lag_door.do_fit(subj_data, 4, 6, 9, 4)
        
        
        outputs =  [get_aic(bias_nllh, 3), get_aic(rl_nllh, 2), get_aic(lag_nllh, 3),
                 get_aic(door_nllh, 3), get_aic(lag_door_nllh, 4), Bev, Blag, Bdoor]
        
        
        f.write(subj+' ')
        for i in outputs:
            f.write(str(i)+' ')
        f.write('\n')
        f.flush()


