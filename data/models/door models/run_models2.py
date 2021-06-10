import os, sys, signal
import exploration_bonus_mixture

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

f = open("modeling results/door_fits2.txt", "w+")

f.write('subj beta phi \n')



n_trials = 100


beh_data = openfile('door_data.txt', n_trials)


def get_aic(nllh, n_params):
    aic = 2*nllh + 2*n_params
    return aic


for subj_data in beh_data[:]:
        
        subj = subj_data[0][0]
            

        [lag_nllh, beta, phi] = exploration_bonus_mixture.do_fit(subj_data, 4, 6, 4)

        
        
        f.write(subj+' ')
        f.write(str(beta)+' ')
        f.write(str(phi)+' ')
        f.write('\n')
        f.flush()


