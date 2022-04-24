import numpy as np
import pandas as pd
import random
import math
import copy
from ofa.model_zoo import ofa_net
from ofa.utils import download_url
from ofa.tutorial import AccuracyPredictor, FLOPsTable, LatencyTable
from evolution_finder_custom import EvolutionFinder, EvolutionFinderQDO

# set random seed
random_seed = 1
random.seed(random_seed)
np.random.seed(random_seed)

ofa_network = ofa_net("ofa_mbv3_d234_e346_k357_w1.2", pretrained=True)

# accuracy predictor
accuracy_predictor = AccuracyPredictor(
    pretrained=True,
    device="cpu"
)

target_hardware = "note10"
latency_table = LatencyTable(device=target_hardware)

qdo_pd = pd.DataFrame()
evo_pd = pd.DataFrame()

# QDO 
# Figure 10, rechts oben https://arxiv.org/pdf/1908.09791.pdf
latency_constraints = [15, 18, 21, 24, 27, 30, 33]  # ms, suggested range [15, 33] ms
P = 100  # The size of population in each generation
N = 500  # How many generations of population to be searched
n = len(latency_constraints)
evals = P + N * P
single_N = math.ceil((evals - n * P) / (n * P))  # Budget for the single runs

for repl in range(100):
    # QDO (MAP-Elites)
    params = {
        'constraint_type': target_hardware, # Let's do latency-constrained search
        'efficiency_constraints': latency_constraints,
        'mutate_prob': 0.1, # The probability of mutation in evolutionary search
        'efficiency_predictor': latency_table, # To use a predefined efficiency predictor
        'accuracy_predictor': accuracy_predictor, # To use a predefined accuracy_predictor predictor
        'population_size': P,
        'max_time_budget': N,
    }
    
    finder = EvolutionFinderQDO(**params)
    elites = finder.run_evolution_search()
   
    # single regularized evolution for each constraint with adjusted budget 
    elites_single = [None] * len(latency_constraints)
    for i in range(len(latency_constraints)):
        r = 0.25  # The ratio of networks that are used as parents for next generation
        params = {
            'constraint_type': target_hardware, # Let's do latency-constrained search
            'efficiency_constraint': latency_constraints[i],
            'mutate_prob': 0.1, # The probability of mutation in evolutionary search
            'mutation_ratio': 0.5, # The ratio of networks that are generated through mutation in generation n >= 2.
            'efficiency_predictor': latency_table, # To use a predefined efficiency predictor.
            'accuracy_predictor': accuracy_predictor, # To use a predefined accuracy_predictor predictor.
            'population_size': P,
            'max_time_budget': single_N,
            'parent_ratio': r,
        }
    
        # build the evolution finder
        finder_single = EvolutionFinder(**params)
        _, best_info = finder_single.run_evolution_search()
        elites_single[i] = best_info
    
    qdo = [x[0] for x in elites]
    qdo_pd = pd.concat([qdo_pd, pd.DataFrame(qdo)], axis = 1)
    
    evo = [x[0] for x in elites_single]
    evo_pd = pd.concat([evo_pd, pd.DataFrame(evo)], axis = 1)

qdo_pd.to_csv("results/qdo.csv", index = False)
evo_pd.to_csv("results/evo.csv", index = False)

###

qdo_pd = pd.DataFrame()
evo_pd = pd.DataFrame()

# QDO 
# Figure 10, rechts oben https://arxiv.org/pdf/1908.09791.pdf
latency_constraints = [15, 18, 21, 24, 27, 30, 33]  # ms, suggested range [15, 33] ms
P = 50  # The size of population in each generation
N = 100  # How many generations of population to be searched
n = len(latency_constraints)
evals = P + N * P
single_N = math.ceil((evals - n * P) / (n * P))  # Budget for the single runs

for repl in range(100):
    # QDO (MAP-Elites)
    params = {
        'constraint_type': target_hardware, # Let's do latency-constrained search
        'efficiency_constraints': latency_constraints,
        'mutate_prob': 0.1, # The probability of mutation in evolutionary search
        'efficiency_predictor': latency_table, # To use a predefined efficiency predictor
        'accuracy_predictor': accuracy_predictor, # To use a predefined accuracy_predictor predictor
        'population_size': P,
        'max_time_budget': N,
    }
    
    finder = EvolutionFinderQDO(**params)
    elites = finder.run_evolution_search()
   
    # single regularized evolution for each constraint with adjusted budget 
    elites_single = [None] * len(latency_constraints)
    for i in range(len(latency_constraints)):
        r = 0.25  # The ratio of networks that are used as parents for next generation
        params = {
            'constraint_type': target_hardware, # Let's do latency-constrained search
            'efficiency_constraint': latency_constraints[i],
            'mutate_prob': 0.1, # The probability of mutation in evolutionary search
            'mutation_ratio': 0.5, # The ratio of networks that are generated through mutation in generation n >= 2.
            'efficiency_predictor': latency_table, # To use a predefined efficiency predictor.
            'accuracy_predictor': accuracy_predictor, # To use a predefined accuracy_predictor predictor.
            'population_size': P,
            'max_time_budget': single_N,
            'parent_ratio': r,
        }
    
        # build the evolution finder
        finder_single = EvolutionFinder(**params)
        _, best_info = finder_single.run_evolution_search()
        elites_single[i] = best_info
    
    qdo = [x[0] for x in elites]
    qdo_pd = pd.concat([qdo_pd, pd.DataFrame(qdo)], axis = 1)
    
    evo = [x[0] for x in elites_single]
    evo_pd = pd.concat([evo_pd, pd.DataFrame(evo)], axis = 1)

qdo_pd.to_csv("results/qdo_small.csv", index = False)
evo_pd.to_csv("results/evo_small.csv", index = False)
