import numpy as np
import pandas as pd

def solve_model(params, max_iter=10000, tol=1e-2):
    """
    Solves the model using Value Function Iteration.

    Args:
        params (dict): A dictionary containing all model parameters.
        max_iter (int): Maximum number of iterations to run.
        tol (float): The tolerance for convergence.

    Returns:
        dict: A dictionary with the converged value functions.
    """
    # Unpack parameters from the dictionary
    wg = params['w_g']
    wfg = params['w_f_g']
    wfb = params['w_f_b']
    beta = params['beta']
    s = params['s']
    p = params['p']
    pg = params['p_g']
    alpha = params['alpha']

    # Initialize guess for the value functions
    Vf_g, Vf_b, V_U, V_G = 0.0, 0.0, 0.0, 0.0

    for i in range(max_iter):
        # Store the old values to check for convergence
        Vf_g_old, Vf_b_old, V_U_old, V_G_old = Vf_g, Vf_b, V_U, V_G

        # --- Update the value functions based on equations ---

        # Value of a Good Job
        Vf_g = wfg + beta * ((1 - s) * np.max([Vf_g_old, V_U_old, V_G_old]) + s * np.max([V_U_old, V_G_old]))

        # Value of a Bad Job
        Vf_b = wfb + wg + beta * ((1 - s) * np.max([Vf_b_old, V_U_old, V_G_old]) + s * np.max([V_U_old, V_G_old]))

        # Value of Unemployment.
        future_val_U = (p * alpha * np.max([Vf_g_old, V_U_old, V_G_old]) +
                        p * (1 - alpha) * np.max([Vf_b_old, V_U_old, V_G_old]) +
                        (1 - p) * np.max([V_U_old, V_G_old]))
        V_U = beta * future_val_U

        # Value of Uber
        future_val_G = (pg * alpha * np.max([Vf_g_old, V_U_old, V_G_old]) +
                        pg * (1 - alpha) * np.max([Vf_b_old, V_U_old, V_G_old]) +
                        (1 - pg) * np.max([V_U_old, V_G_old]))
        V_G = wg + beta * future_val_G
        
        # --- Check for convergence ---
        error = np.max([abs(Vf_g - Vf_g_old), abs(Vf_b - Vf_b_old),
                        abs(V_U - V_U_old), abs(V_G - V_G_old)])

        if error < tol:
            # print(f"Converged after {i+1} iterations.")
            return {'Vf_g': Vf_g, 'Vf_b': Vf_b, 'V_U': V_U, 'V_G': V_G, 'p' : p, 'wfg': wfg, 'w_g': wg}

    print("Warning: Model did not converge within the maximum number of iterations.")
    return {'Vf_g': Vf_g, 'Vf_b': Vf_b, 'V_U': V_U, 'V_G': V_G, 'p' : p, 'wfg': wfg, 'w_g': wg}

# Applying the code to solve the model with a set of parameters.
# Define parameters that we will change to test the model
wage_good = np.arange(2000, 10001, 100)  # Good job wage range
wage_uber = [0,1500, 2001, 2500] # Uber wage

# Iterate over the parameters to solve the model for each combination
results = []
for wfg in wage_good:
    for wg in wage_uber:
        params = {
            'w_g': wg,     # Uber monthly income
            'w_f_g': wfg,    # Good job monthly wage
            'w_f_b': 1200,   # Bad job monthly wage
            'beta': 0.99,    # Discount factor
            's': 0.06,       # Probability of being laid off a formal job
            'p': 0.2,       # Probability of finding a job when unemployed
            'p_g': 0.1,      # Probability of finding a job when driving for Uber
            'alpha': 0.4     # Probability that a job offer is a "Good Job"
        }
        result = solve_model(params)
        results.append(result)

#plot the value functions for each combination of wfg and p
import matplotlib.pyplot as plt

# Convert results to a DataFrame for easier plotting
df = pd.DataFrame(results)

#plot
fig, axs = plt.subplots(2, 2, figsize=(14, 10), sharex=True, sharey=True)
wg_values = sorted(df['w_g'].unique())
value_functions = ['Vf_g', 'Vf_b', 'V_U', 'V_G']

for idx, wg in enumerate(wg_values):
    ax = axs[idx // 2, idx % 2]
    subset = df[df['w_g'] == wg]
    for vf in value_functions:
        ax.plot(subset['wfg'], subset[vf], label=vf)
    ax.set_title(f'Uber wage (w_g) = {wg}')
    ax.set_xlabel('Good job wage (wfg)')
    ax.set_ylabel('Value function')
    ax.legend()
    ax.grid(True)
    ax.set_xlim(right=5000)
    ax.set_ylim(top=400000)

plt.tight_layout()
plt.show()


