import numpy as np
from get_data import get_data
import random

class Problem:
    """Define the problem class with the objective function and constraints

    Args:
        num_variables (int): Number of variables in the problem
        liquidity_constraint (float): The constraint on the sum of the variables

    """
    def __init__(self, num_variables, liquidity_constraint):
        self.num_variables = num_variables
        self.liquidity_constraint = liquidity_constraint

    def evaluate(self, solution):
        # Define your objective functions here
        f1 = np.sum(solution)
        return f1

    def is_feasible(self, solution):
        "Checking if the solution meets the constraint"
        return np.sum(solution) <= self.liquidity_constraint

def initialize_population(pop_size, num_variables):
    return [np.random.choice([0,1], size=num_variables) for _ in range(pop_size)]

def evaluate_population(population, problem):
    return [problem.evaluate(ind) for ind in population]

def select_parents(population, fitness):
    # Implement tournament selection or any other selection method
    return random.sample(population, 2)

def crossover(parent1, parent2):
    # Implement crossover logic
    point = random.randint(1, len(parent1) - 1)
    child1 = np.concatenate((parent1[:point], parent2[point:]))
    child2 = np.concatenate((parent2[:point], parent1[point:]))
    return child1, child2

def mutate(solution, mutation_rate=0.1):
    for i in range(len(solution)):
        if random.random() < mutation_rate:
            solution[i] = random.random()
    return solution

def non_dominated_sorting(population, fitness):
    # Implement non-dominated sorting
    pass

def crowding_distance(population, fitness):
    # Implement crowding distance calculation
    pass

def nsga2(pop_size, num_generations, num_variables, liquidity_constraint):
    problem = Problem(num_variables, liquidity_constraint)
    population = initialize_population(pop_size, num_variables)
    fitness = evaluate_population(population, problem)

    for generation in range(num_generations):
        new_population = []
        while len(new_population) < pop_size:
            parent1, parent2 = select_parents(population, fitness)
            child1, child2 = crossover(parent1, parent2)
            child1 = mutate(child1)
            child2 = mutate(child2)
            if problem.is_feasible(child1):
                new_population.append(child1)
            if problem.is_feasible(child2):
                new_population.append(child2)
        
        population.extend(new_population)
        fitness = evaluate_population(population, problem)
        population = non_dominated_sorting(population, fitness)
        population = crowding_distance(population, fitness)
        population = population[:pop_size]

    return population


def non_dominated_sorting(population, fitness):
    num_individuals = len(population)
    domination_count = [0] * num_individuals
    dominated_solutions = [[] for _ in range(num_individuals)]
    fronts = [[]]

    for i in range(num_individuals):
        for j in range(num_individuals):
            if i != j:
                if dominates(fitness[i], fitness[j]):
                    dominated_solutions[i].append(j)
                elif dominates(fitness[j], fitness[i]):
                    domination_count[i] += 1
        if domination_count[i] == 0:
            fronts[0].append(i)

    current_front = 0
    while fronts[current_front]:
        next_front = []
        for i in fronts[current_front]:
            for j in dominated_solutions[i]:
                domination_count[j] -= 1
                if domination_count[j] == 0:
                    next_front.append(j)
        current_front += 1
        fronts.append(next_front)

    return fronts[:-1]

def dominates(ind1, ind2):
    return all(x <= y for x, y in zip(ind1, ind2)) and any(x < y for x, y in zip(ind1, ind2))

# Example usage
population = [
    [0.1, 0.2],
    [0.3, 0.4],
    [0.2, 0.1],
    [0.4, 0.3]
]

fitness = [
    [1, 2],
    [2, 3],
    [1.5, 1],
    [3, 2.5]
]

# fronts = non_dominated_sorting(population, fitness)
# print(fronts)

# Parameters
pop_size = 100
num_generations = 50
num_variables = 10
liquidity_constraint = 5.0

# Run NSGA-II
# final_population = nsga2(pop_size, num_generations, num_variables, liquidity_constraint)


if __name__ == "__main__":
    population = initialize_population(465,85)

    print(population[0])