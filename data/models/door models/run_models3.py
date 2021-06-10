import os, sys, signal
import RL, Lag_only, Door_only


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

f = open("modeling results/separate_model_fits-adults.txt", "w+")

f.write('subj RL_llh B_value Lag_llh B_lag Door_llh B_door \n')



n_trials = 100


#beh_data = openfile('door_data.txt', n_trials)
beh_data = openfile('door_data_adults.txt', n_trials)


def get_aic(nllh, n_params):
    aic = 2*nllh + 2*n_params
    return aic


for subj_data in beh_data[:]:
        
        subj = subj_data[0][0]
            

        [RL_nllh, B_value] = RL.do_fit(subj_data, 4, 6, 4)
        [Lag_nllh, B_lag] = Lag_only.do_fit(subj_data, 4, 6, 4)
        [Door_nllh, B_door] = Door_only.do_fit(subj_data, 4, 6, 9, 4)

        outputs = [subj, RL_nllh, B_value, Lag_nllh, B_lag, Door_nllh, B_door]
        
        for thing in outputs:
        	f.write(str(thing)+'\t')
        f.write('\n')
        f.flush()


