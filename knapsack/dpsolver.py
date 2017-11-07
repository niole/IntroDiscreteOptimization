def dpsolver(items, K):
    space = fill_space(items, K)
    solution_x = len(space[0]) - 1
    solution_y = len(space) - 1
    o_func = space[solution_y][solution_x]

    solution = ["0" for _ in items]

    y = solution_y
    x = solution_x
    while x > 0:
        if space[y][x-1] != space[y][x]:
            solution[x] = "1"
            y -= items[x].weight

        x -= 1

    if space[y][x] > 0:
        solution[x] = "1"

    return str(o_func) + " 0\n" + " ".join(solution)

def fill_space(items, K):
    space = [[0 for _ in items] for _ in range(K+1)]

    for item in items:
        for k in range(K+1):
            if item.weight <= k:
                if item.index > 0:
                    # special
                    upstream = space[k-item.weight][item.index - 1]
                    left = space[k][item.index - 1]
                    space[k][item.index] = max(item.value + upstream, left)
                else:
                    space[k][item.index] = item.value
            elif item.index > 0:
                space[k][item.index] = space[k][item.index - 1]

    return space
