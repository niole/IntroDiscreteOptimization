from collections import namedtuple
from random import random
from functools import reduce

Item = namedtuple("Item", ['index', 'value', 'weight'])
Best = namedtuple("Best", ['capacity', 'value', 'best_guess', 'chosen'])

def get_objective_func(available_items, K):
    weighted = [(float(item.value)/item.weight, item) for item in available_items]
    w_density = sorted(weighted, key=lambda t: t[0], reverse=True)

    weight_so_far = 0
    value = 0
    i = 0

    while i < len(w_density) and weight_so_far < K:
        next_item = w_density[i]
        next_weight = next_item[1].weight
        next_value = next_item[1].value

        if weight_so_far + next_weight <= K:
            weight_so_far += next_weight
            value += next_value

        elif weight_so_far + next_weight > K:
            diff = float(K - weight_so_far)
            multiplier = diff/next_weight
            weight_so_far = K
            value += multiplier * next_value

        i += 1
    return value

def get_better(b1, b2):
    if b1.value > b2.value:
        return b1
    return b2

"""
items - Item[]
obj_func - best value guess
K - remaining capacity in bag
V - total current value in bag
currentBest - a current best, Best
currentIndex - currently selected item
chosen - a list of indexes determining what has been selected
"""
def solver(items, obj_func, K, V, currentBest, currentIndex, chosen):
#    print("obj_func: %s, K: %s, V: %s, current best: %s, current Index: %s, chosen: %s" % (obj_func, K, V, currentBest.chosen, currentIndex, chosen))
    if K < 0:
        return
    elif K == 0 or currentIndex == len(items):
        return currentBest
    elif K > 0 and currentIndex < len(items):
        k = K - items[currentIndex].weight
        v = V + items[currentIndex].value

        if v >= currentBest.value or obj_func >= currentBest.best_guess:

            rightChosen = chosen + ["1"]
            nextBest = get_better(Best(k, v, obj_func, rightChosen), currentBest)
            best = solver(items, obj_func, k, v, nextBest, currentIndex+1, rightChosen)

            leftChosen = chosen + ["0"]
            obj_func_without = get_objective_func(get_available_items(items, leftChosen), K)

            if best:
                return solver(items, obj_func_without, K, V, best, currentIndex+1, leftChosen)
            return solver(items, obj_func_without, K, V, currentBest, currentIndex+1, leftChosen)

        elif obj_func < currentBest.best_guess and k < currentBest.value:
            return currentBest
#def solver(items, obj_func, K, V, currentBest, currentIndex, chosen):
##    print("obj_func: %s, K: %s, V: %s, current best: %s, current Index: %s, chosen: %s" % (obj_func, K, V, currentBest.chosen, currentIndex, chosen))
#    if K < 0:
#        return
#    elif K == 0 or currentIndex == len(items):
#        return currentBest
#    elif K > 0 and currentIndex < len(items):
#        k = K - items[currentIndex].weight
#        v = V + items[currentIndex].value
#
#        if v >= currentBest.value or obj_func >= currentBest.best_guess:
#
#            rightChosen = chosen + ["1"]
#            nextBest = get_better(Best(k, v, obj_func, rightChosen), currentBest)
#            best = solver(items, obj_func, k, v, nextBest, currentIndex+1, rightChosen)
#
#            leftChosen = chosen + ["0"]
#            obj_func_without = get_objective_func(get_available_items(items, leftChosen), K)
#
#            if best:
#                return solver(items, obj_func_without, K, V, best, currentIndex+1, leftChosen)
#            return solver(items, obj_func_without, K, V, currentBest, currentIndex+1, leftChosen)
#
#        elif obj_func < currentBest.best_guess and k < currentBest.value:
#            return currentBest

def format_chosen(items, chosen):
    return " ".join(chosen)

def get_available_items(items, chosen):
    uptil = [x[0] for x in zip(items, chosen) if x[1] == "1"]
    after = items[len(chosen):]
    return uptil + after

def branchboundsolver(items, K):
    """
    left is picked, right is not
    each time not, reevaluate objective function
    if objective function is lower than highest obj func, discount subtree
    if reach leaf and obj func === best obj func, but value is lower, discount

    at all times must know current best, always reevaluate obj func when going right
    """
    objective_f = get_objective_func(items, K)
    best = solver(items, objective_f, K, 0, Best(K, 0, objective_f, []), 0, [])
    formatted_chosen = format_chosen(items, best.chosen)
    return "%s %s\n%s" % (best.value, 1, formatted_chosen)


items = [
    Item(0, 4, 1),
    Item(1, 1, 2),
    Item(2, 2, 3),
    Item(3, 5, 3)
]

K = 6

#print(branchboundsolver(items, K))
