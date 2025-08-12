import numpy as np
import pandas as pd
import math

def prob_uber(wf, mid_wage):
    """
    Calculates the probability of being able to work for uber

    Args:
        wf (float): Wage from a good job.
        mid_wage (float): Wage at which workers have a 50% chance of being able to work for Uber.

    Returns:
        float: Probability of finding a job while driving for Uber.
    """
    prob = 1/(1+math.exp(-(wf-mid_wage)/1000))
    return(prob)



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
    wf = params['w_f']
    beta = params['beta']
    c = params['prob_out']
    disp = params['prob_disp']
    phi_g = params['phi_uber']
    phi_u = params['phi_unemployed']
    mid_wage = params['wage_mid'] 

    # Initialize guess for the value functions
    V_f, V_U, V_G = 0.0, 0.0, 0.0

    for i in range(max_iter):
        # Store the old values to check for convergence
        V_f_old, V_U_old, V_G_old = V_f, V_U, V_G

        # --- Update the value functions based on equations ---

        # Value of a Good Job
        future_value_f = ((1-disp) * V_f_old + 
                          disp * (prob_uber(wf, mid_wage) * max(V_G_old, V_U_old) +
                                   (1-prob_uber(wf, mid_wage)) * V_U_old))
        V_f = wf + beta * (1-c) * future_value_f

        # Value of Unemployment.
        V_U = 0 + beta * (1-c) * (phi_u * max(V_f_old, V_U_old) + (1-phi_u) * V_U_old)

        # Value of Uber
        V_G = wg + beta * (1-c) * (phi_g * max(V_f_old, V_G_old) + (1-phi_g) * V_G_old)
        
        # --- Check for convergence ---
        error = np.max([abs(V_f - V_f_old), abs(V_U - V_U_old), abs(V_G - V_G_old)])

        if error < tol:
            # print(f"Converged after {i+1} iterations.")
            return {'V_f': V_f, 'V_U': V_U, 'V_G': V_G, 'c' : c, 'wf': wf, 'wg': wg}

    print("Warning: Model did not converge within the maximum number of iterations.")
    return {'V_f': V_f, 'V_U': V_U, 'V_G': V_G, 'c' : c, 'wf': wf, 'wg': wg}



# Applying the code to solve the model with a set of parameters.
# Define parameters that we will change to test the model
wage_formal = np.arange(1000, 10001, 100)  # Good job wage range

# Iterate over the parameters to solve the model for each combination
results = []
for wage in wage_formal:
    params = {
            'w_g': 3500,     # Uber monthly income
            'w_f': wage,    # Good job monthly wage
            'beta': 0.99,    # Discount factor
            'prob_out': 0, #1/((65-20)*12),  # Probability of retirement
            'prob_disp': 0.01, # Probability of being laid off a formal job
            'phi_uber': 0.1,  # Probability of finding a job when driving for Uber
            'phi_unemployed': 0.2,  # Probability of finding a job when unemployed
            'wage_mid': 5000  # Mid wage for Uber job probability
        }
    result = solve_model(params)
    results.append(result)


#PLOT
import matplotlib.pyplot as plt

# Convert results to a DataFrame for easier plotting
df_results = pd.DataFrame(results)
fig, ax = plt.subplots(figsize=(12, 6), layout='constrained')
ax.plot(df_results['wf'], df_results['V_f'], label='Value of Formal Job (V_f)', color='blue')
ax.plot(df_results['wf'], df_results['V_U'], label='Value of Unemployment (V_U)', color='orange')
ax.plot(df_results['wf'], df_results['V_G'], label='Value of Uber (V_G)', color='green')
ax.set_ylabel('Value')
ax.set_xlabel('Wage from Good Job (w_f)')
ax.legend()
plt.show()




#############Simulação

def simulate_economy(params, n_pop=100000, initial_unemp_rate=0.1, max_sim_periods=200, sim_tol=1e-6):
    """
    Simula a dinâmica de uma população de acordo com o modelo.

    Args:
        params (dict): Dicionário com todos os parâmetros do modelo.
        n_pop (int): Tamanho total da população a ser simulada.
        initial_unemp_rate (float): Taxa de desemprego inicial (ex: 0.05 para 5%).
        max_sim_periods (int): Número máximo de períodos para a simulação.
        sim_tol (float): Tolerância para verificar a convergência do estado estacionário.

    Returns:
        pandas.DataFrame: Um DataFrame com a trajetória do número de pessoas em cada estado.
    """
    # 1. Resolver o modelo para obter as funções valor e determinar as escolhas ótimas
    value_functions = solve_model(params)
    V_f, V_U, V_G = value_functions['V_f'], value_functions['V_U'], value_functions['V_G']

    # Determina as escolhas ótimas dos agentes com base nos valores
    # O que um demitido faz se PUDER ir pra Uber?
    choice_layoff_is_uber = V_G > V_U
    # Um desempregado aceita uma oferta formal?
    choice_unemployed_accepts_job = V_f > V_U
    # Um motorista de Uber aceita uma oferta formal?
    choice_uber_accepts_job = V_f > V_G

    # Desempacota parâmetros necessários para as transições
    disp = params['prob_disp']
    phi_u = params['phi_unemployed']
    phi_g = params['phi_uber']
    prob_can_uber = prob_uber(params['w_f'], params['wage_mid'])

    # 2. Inicializar a população no período 0
    U_t = n_pop * initial_unemp_rate
    E_t = n_pop - U_t
    G_t = 0.0

    # Armazena a trajetória
    trajectory = [{'Periodo': 0, 'Empregados (E)': E_t, 'Desempregados (U)': U_t, 'Uber (G)': G_t, 
                   'Share E': E_t/n_pop, 'Share U': U_t/n_pop, 'Share G': G_t/n_pop}]

    # 3. Rodar a simulação
    for t in range(max_sim_periods):
        # Armazena os valores do período anterior para checar convergência
        E_prev, U_prev, G_prev = E_t, U_t, G_t

        # --- Calcular os fluxos de um estado para outro ---

        # Fluxos saindo de Empregados (E)
        newly_laid_off = E_t * disp
        
        # Desses demitidos, quantos podem ir para a Uber?
        flow_E_to_G = newly_laid_off * prob_can_uber * (1 if choice_layoff_is_uber else 0)
        flow_E_to_U = newly_laid_off * (1 - prob_can_uber) + \
                      newly_laid_off * prob_can_uber * (0 if choice_layoff_is_uber else 1)

        # Fluxos saindo de Desempregados (U)
        flow_U_to_E = U_t * phi_u * (1 if choice_unemployed_accepts_job else 0)

        # Fluxos saindo da Uber (G)
        flow_G_to_E = G_t * phi_g * (1 if choice_uber_accepts_job else 0)

        # --- Calcular os novos totais para o período t+1 ---
        E_t1 = E_t + flow_U_to_E + flow_G_to_E - newly_laid_off
        U_t1 = U_t + flow_E_to_U - flow_U_to_E
        G_t1 = G_t + flow_E_to_G - flow_G_to_E 
        
        E_t, U_t, G_t = E_t1, U_t1, G_t1

        trajectory.append({'Periodo': t + 1, 'Empregados (E)': E_t, 'Desempregados (U)': U_t, 'Uber (G)': G_t,
         'Share E': E_t/n_pop, 'Share U': U_t/n_pop, 'Share G': G_t/n_pop})

        # 4. Checar convergência
        share_E = E_t / n_pop
        share_U = U_t / n_pop
        share_G = G_t / n_pop
        
        share_E_prev = E_prev / n_pop
        share_U_prev = U_prev / n_pop
        share_G_prev = G_prev / n_pop

        error_share = max(abs(share_E - share_E_prev), abs(share_U - share_U_prev), abs(share_G - share_G_prev))

        if error_share < sim_tol:
            print(f"Convergência atingida no período {t+1}.")
            break
            
    if t == max_sim_periods - 1:
        print("Aviso: A simulação atingiu o número máximo de períodos sem convergir.")

    return pd.DataFrame(trajectory)

def plot_trajectory(df_trajectory):
    """Função auxiliar para plotar os resultados da simulação."""
    df_plot = df_trajectory.set_index('Periodo')
    
    # Converte para percentual
    df_plot = df_plot / df_plot.sum(axis=1).values[0] * 100
    
    plt.style.use('seaborn-v0_8-whitegrid')
    fig, ax = plt.subplots(figsize=(12, 7))
    
    ax.plot(df_plot.index, df_plot['Empregados (E)'], label='Empregados Formais (%)', lw=2.5)
    ax.plot(df_plot.index, df_plot['Desempregados (U)'], label='Desempregados (%)', lw=2.5)
    ax.plot(df_plot.index, df_plot['Uber (G)'], label='Motoristas de Uber (%)', lw=2.5)
    
    ax.set_title('Dinâmica Populacional do Mercado de Trabalho', fontsize=16)
    ax.set_xlabel('Período', fontsize=12)
    ax.set_ylabel('Share da População (%)', fontsize=12)
    ax.legend(fontsize=11)
    ax.grid(True)
    plt.tight_layout()
    plt.show()


#Cenário para ser simulado
params_cenario = {
        'w_g': 3500,               # Renda mensal na Uber
        'w_f': 1500,               # Salário formal mensal
        'beta': 0.99,              # Fator de desconto
        'prob_out': 1/(40*12),     # Probabilidade de sair do mercado (aposentadoria)
        'prob_disp': 0.01,         # Probabilidade de demissão do emprego formal
        'phi_uber': 0.1,           # Probabilidade de achar emprego formal sendo da Uber
        'phi_unemployed': 0.2,     # Probabilidade de achar emprego formal estando desempregado
        'wage_mid': 5000           # Salário mediano para a prob. de poder ser Uber
    }


# Roda a simulação
trajetoria_df = simulate_economy(params=params_cenario, 
                                    n_pop=100000, 
                                    initial_unemp_rate=0.1,
                                    max_sim_periods = 20000)

plot_trajectory(trajetoria_df)