import numpy as np

# Function for input initial data
def input_data():
    # Input numbers of variables and constraints
    n = int(input("Input number of variables: "))
    m = int(input("Input number of constraints: "))

    # Input all the coefficients that define the LPP problem
    objective_coefficients = list(map(int, input("Input vector of coefficients of objective function ").split()))
    constraints = []
    print("Input matrix of coefficients of constraint function")
    for i in range(m):
        row = list(map(int, input().split()))
        constraints.append(row)
    right_coefficient = list(map(int, input("Input vector of right-hand side numbers: ").split()))
    eps = float(input("Input approximation accuracy: "))
    return (n, m, objective_coefficients, constraints, right_coefficient, eps)

# Check if the initial data are corrected
def check_if_applicable(n, m, C, A, b):
    C_columns = len(C)
    A_rows, A_columns = len(A), len(A[0])
    b_rows = len(b)
    if A_rows != m or b_rows != m:
        return False
    if C_columns != n or A_columns != n:
        return False
    return True

# Function for adding slack variables
def add_slack_variables(n, m, C, A):
    for row in range(m):
        C.append(0)
        for cur_row in range(m):
            if cur_row == row:
                A[cur_row].append(1)
            else:
                A[cur_row].append(0)
    return (len(A[0]), len(A), C, A)

# Finding initial decision variables
def find_initial_solution(old_n, n, m, A, b):
    min_coef = min(b)
    max_den = 2 * max(sum([abs(elem) for elem in row]) for row in A)
    initial_x = []
    initial_value_for_non_slack = min_coef / max_den
    for var in range(old_n):
        initial_x.append(initial_value_for_non_slack)
    for var in range(old_n, n):
        summ_var, cur_b = 0, 0
        for row in range(m):
            if A[row][var] == 1:
                summ_var = sum(A[row]) - 1
                cur_b = b[row]
                break
        initial_x.append(cur_b - initial_value_for_non_slack*summ_var)
    return initial_x

'''
For given objective function and constraints. The constraints must be represented as follows:
1) a * x + ... + b * z <= k
...
n) v * x + ... + c * y <= p

It is assumed that all the variables must be greater than zero!
 
The Simplex Matrix solve the problem of maximization by default!
'''
def interior_point_algorithm():
    old_n, m, C, A, b, eps = input_data()
    if not check_if_applicable(old_n, m, C, A, b):
        print("The method is not applicable!")
        return
    if min(b) <= 0:
        print("The problem does not have solution!")
        return
    n, m, C, A = add_slack_variables(old_n, m, C, A)
    x = find_initial_solution(old_n, n, m, A, b)
    C = np.array(C)
    A = np.array(A)
    b = np.array(b)
    x = np.array(x)
    alpha = 0.2
    upper_bound = 10**50
    while True:
        cur = x
        D = np.diag(x)
        A_scaled = np.dot(A, D)
        C_scaled = np.dot(D, np.transpose(C))
        component = np.linalg.inv(np.dot(A_scaled, np.transpose(A_scaled)))
        P = np.identity(n, np.float32) - np.dot(np.dot(np.transpose(A_scaled), component), A_scaled)
        Cp = np.dot(P, np.transpose(C_scaled))
        v = np.absolute(np.min(Cp))
        x_scaled = np.ones(n, np.float32) + (alpha / v) * Cp
        new_x = np.dot(D, np.transpose(x_scaled))
        x = new_x
        max_value = max(x)
        if max_value > upper_bound:
            print("The problem does not have solution!")
            return
        if np.linalg.norm(cur - new_x, ord=2) < eps:
            break
    print("Decision:")
    for i in range(old_n):
        print(f"x{i+1} = {x[i]}")
    print(f"Objective function z = {sum([x[i]*C[i] for i in range(old_n)])}")

interior_point_algorithm()
