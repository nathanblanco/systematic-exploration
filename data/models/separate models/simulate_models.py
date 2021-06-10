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


def simulate_RL(subj, B_value, n_trials, n_sims):
		
	alpha = 1.0


	#('subj', 'phase', 'trial', 'cond', 'res', 'rt', 'reward', 'total', 'image', 'door_loc', 'door_chose', 'r1', 'r2', 'r3', 'r4')

	outputs = []

	rewards = [10, 3, 2, 1]
	

	for sim in range(n_sims):
	
		shuffle(rewards)
	
		prev_res = -1
		
		Q = array([0.0]*4)
	
		lag = array([1]*4)

		for trial in range(n_trials):
	
			door_loc = randint(0,3)
			
			numerators = [ exp(i*B_value) for i in Q]
			denominator = sum(numerators)
			probs = [ i/denominator for i in numerators]
	
			choice_rand = random()
	
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
	
			Q[res] += alpha * ( payoff - Q[res] ) 
	
# 			print '=============================='
# 			print Q
# 			print '-------------------------'
# 			print probs
# 			print '-------------------------'
# 			print choice_rand
# 			print '-------------------------'
# 			print res
# 			print '-------------------------'

			stay = int(prev_res == res)
	
			outputs.append([subj, 'RL', B_value, sim, (subj*1000)+sim, trial, res, payoff, chose_door, stay, lag[res], rewards[prev_res]])
			
			lag = [i+1 for i in lag]
			lag[res] = 1
	
			prev_res = res
			
	return outputs

def simulate_Lag(subj,  Blag, n_trials, n_sims):

	

	outputs = []

	rewards = [10, 3, 2, 1]


	for sim in range(n_sims):
	
		lag = array([1]*4)
	
		shuffle(rewards)

		prev_res = -1
		
		for trial in range(n_trials):
		
			door_loc = randint(0,3)

			numerators = [ exp(Blag*lag[i]) for i in range(len(lag))]
			denominator = sum(numerators)
			probs = [ i/denominator for i in numerators]
	
			choice_rand = random()
	
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
			
# 			print '=============================='
# 			print lag
# 			print '-------------------------'
# 			print probs
# 			print '-------------------------'
# 			print choice_rand
# 			print '-------------------------'
# 			print res
# 			print '-------------------------'

			stay = int(prev_res == res)
			
			outputs.append([subj, 'Lag', Blag, sim, (subj*1000)+sim, trial, res, payoff, chose_door, stay, lag[res], rewards[prev_res]])
			
			lag = [i+1 for i in lag]
			lag[res] = 1
			
			
	
			prev_res = res

	return outputs
	
def simulate_random(subj,  n_trials, n_sims):


	outputs = []
	rewards = [10, 3, 2, 1]
	
	lag = array([1]*4)

	
	probs = [0.25, 0.25, 0.25, 0.25]

	for sim in range(n_sims):
		shuffle(rewards)
		prev_res = -1
		lag = array([1]*4)
		
		for trial in range(n_trials):
		
			door_loc = randint(0,3)
	
			choice_rand = random()
	
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
			
		
			
			outputs.append([subj, 'Random', -1, sim, (subj*1000)+sim, trial, res, payoff, chose_door, stay, lag[res], rewards[prev_res]])
			
			prev_res = res
			
			lag = [i+1 for i in lag]
			lag[res] = 1

	return outputs
	

def simulate_Door(subj, Bdoor, n_trials, n_sims):

	outputs = []

	rewards = [10, 3, 2, 1]
	
	lag = array([1]*4)


	for sim in range(n_sims):
		shuffle(rewards)
		prev_res = -1
		lag = array([1]*4)
		door = array([0]*4)
		
		for trial in range(n_trials):
		
			door_loc = randint(0,3)
			
			door[door_loc] = 1
		
			numerators = [ exp(Bdoor*door[i]) for i in range(len(door))]
			denominator = sum(numerators)
			probs = [ i/denominator for i in numerators]
	
			choice_rand = random()
	
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
			
			
			outputs.append([subj, 'Door', Bdoor, sim, (subj*1000)+sim, trial, res, payoff, chose_door, stay, lag[res], rewards[prev_res]])
			
			prev_res = res
			
			lag = [i+1 for i in lag]
			lag[res] = 1
			
			door = array([0]*4)

	return outputs



def do_sims(datafile, outputfile, n_trials, n_sims):

	subj_data = openfile(datafile)
	
	f = open(outputfile, 'w+')
	
	# subj RL_llh B_value Lag_llh B_lag 
	# subj RL_llh B_value Lag_llh B_lag Door_llh B_door 
	for line in subj_data[1:]:
		subj =  int(line[0])
		B_value = float(line[2])
		B_lag = float(line[4])

		
		rl_outputs = simulate_RL(subj, B_value, n_trials, n_sims)
		
		lag_outputs = simulate_Lag(subj, B_lag, n_trials, n_sims)
		
		rand_outputs = simulate_random(subj, n_trials, n_sims)

		if len(line) == 7:

			B_door = float(line[6])
			
			door_outputs = simulate_Door(subj, B_door, n_trials, n_sims)
			
			for line in door_outputs:
				for thing in line:
					f.write( str(thing)+'\t')
				f.write('\n')
				f.flush()
		
		for line in rl_outputs:
			for thing in line:
				f.write( str(thing)+'\t')
			f.write('\n')
			f.flush()
			
		for line in lag_outputs:
			for thing in line:
				f.write( str(thing)+'\t')
			f.write('\n')
			f.flush()
			
		for line in rand_outputs:
			for thing in line:
				f.write( str(thing)+'\t')
			f.write('\n')
			f.flush()
			

			
#--------------------------------------------------------------#

n_trials = 100
n_sims = 100

datafile = 'modeling results/separate_model_fits-adults.txt'
outputfile = 'modeling results/sims/adults.txt'

do_sims(datafile, outputfile, n_trials, n_sims)

datafile = 'modeling results/separate_model_fits-kids.txt'
outputfile = 'modeling results/sims/kids.txt'

do_sims(datafile, outputfile, n_trials, n_sims)

datafile = 'modeling results/separate_model_fits-bland.txt'
outputfile = 'modeling results/sims/bland.txt'

do_sims(datafile, outputfile, n_trials, n_sims)


datafile = 'modeling results/separate_model_fits-door.txt'
outputfile = 'modeling results/sims/door.txt'

do_sims(datafile, outputfile, n_trials, n_sims)


