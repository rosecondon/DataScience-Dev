import simpy
import random

# Senario : Build a simulation of the system, and then vary the number of ID/boarding-pass checkers and personal-check queues 
# to determine how many are needed to keep average wait times below 15 minutes.

# In airport, boarding pass takes place first, then customer will go to personal security check. So personal security check will wait for ID check
# process completed. Therefore, we need a resource to an Environment and a capacity.

# so, passenger arrives at the checkers at a random time, if one checker is available, passenger will start ID/boarding check and wait for the checking completed.
# if no checker available, they have to wait for the next one available. Wait time is 15 mins

RANDOM_SEED = 42
NUM_CHECKERS = 5  # Number of ID/boarding checker
WAITINGTIME = 15  # maximum passenger waiting time
T_INTER = 5  	  # Ever 7 mintues one passenger arrives
SIM_TIME = 20     # Simulation time in minutes
CHECKTIME = 2     # Minutes it takes to check 1 passenger boarding

# System : Airport	Agent : Passenger	Process : boarding-pass checking
class Airportchecker(object):
	"""An airport has a limited number of checkers (``NUM_CHECKERS``) for boarding check in parallel.
Passengers have to wait for one checker available. When they got one, they can start the boarding processes and wait for it to finish (which
takes ``CHECKTIME``
"""

	def __init__(self, env, num_checkers, checktime):
		self.env = env
		self.checker = simpy.Resource(env, num_checkers)
		self.checktime = checktime

	def check(self, passenger):
		yield self.env.timeout(CHECKTIME)
		print("Checkers completed %d%% of %s' checkers passengers." % (random.randint(50, 99), passenger))

def passenger(env, name, airportchecker):
	"""The car process (each car has a ``name``) arrives at the airport (``airportchecker``) and requests a boarding checker.
	It then starts the checking process, waits for it to finish and leave ...
	"""
	print('%s arrives at the airport at %.2f.' % (name, env.now)) 
	with airportchecker.checker.request() as request:
		yield request
		print('%s enters the airport checker at %.2f.' % (name, env.now))
		yield env.process(airportchecker.check(name))
		print('%s leaves the checker at %.2f.' % (name, env.now))

def setup(env, num_checkers, checktime, t_inter):
	airportchecker = Airportchecker(env, num_checkers, checktime)

	for i in range(4):
		env.process(passenger(env, 'Passenger %d' % i, airportchecker))

	while True:
		yield env.timeout(random.randint(t_inter - 2, t_inter + 2)) 
		i += 1
		env.process(passenger(env, 'Passenger %d' % i, airportchecker))

def main():
    # Setup and start the simulation
    random.seed(RANDOM_SEED)

    # Run the simulation
    env = simpy.Environment()
    env.process(setup(env, NUM_CHECKERS, CHECKTIME, T_INTER))
    env.run(until=SIM_TIME)
	
    print("Running Airportchecker simulation ...")
    print('========================') 


if __name__ == "__main__":
    main()
