#Patent with Foreign and Domestic Inventor

import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import gaussian_kde, truncnorm, norm
import pandas as pd
import os
from scipy.special import erf, owens_t
from scipy.stats import norm
from scipy.special import owens_t

# Parameters


# Data directory
wrd =r"C:\Users\jayho\OneDrive - Duke University\ABF_Replication"

# Read the Stata file

#Save Pat Pub Citation Pair File
filename =os.path.join(wrd,r"Data\appendix\patent_pythonallnonusa_invt.dta")
print(filename)

data = pd.read_stata(filename)


#data = pd.read_stata(r"C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research\Data\patent_science_python_jay.dta")


from scipy.special import owens_t as scipy_owens_t

def E_science_estimate(R, sigma_u):
    """
    Estimate E(v|R) using provided expression
    :param R: float
        Input R value
    :param theta1: float
        Parameter theta1
    :param theta2: float
        Parameter theta2
    :param sigma_u: float
        Parameter sigma_u
    :return: float
        Estimated value of E(v|R)
    """
    theta1 = np.sqrt(np.exp(.0175016)-1)
    theta2 = np.sqrt(np.exp( .0194975 )-1)
    s = sigma_u * np.sqrt(1 + theta1**2 + theta2**2)
    
    omega1 = s * np.sqrt(1 + theta2**2) / theta1
    omega2 = s * np.sqrt(1 + theta1**2) / theta2
    
    lambda1 = (theta2 / theta1) * np.sqrt(1 + theta1**2 + theta2**2)
    lambda2 = (theta1 / theta2) * np.sqrt(1 + theta1**2 + theta2**2)
    
    phi = norm.pdf
    Phi = norm.cdf
    
    numerator = (2 * ((1 + theta2 ** 2) * sigma_u ** 2 / omega1) * phi(R / omega1) * Phi(lambda1 / omega1 * R) -
                 2 * theta1 ** 2 * sigma_u ** 2 / omega2 * phi(R / omega2) * Phi(lambda2 / omega2 * R))
    
    denominator = (Phi(R / omega1) - 2 * scipy_owens_t(R / omega1, lambda1) +
                   Phi(R / omega2) - 2 * scipy_owens_t(R / omega2, lambda2))
    
    return (theta1 ** 2 / (1 + theta1 ** 2 + theta2 ** 2)) * R + numerator / denominator





def E_nonscience_estimate(R, sigma_u):
    """
    Estimate E(v|R) using provided expression
    :param R: float
        Input R value
    :param theta1: float
        Parameter theta1
    :param theta2: float
        Parameter theta2
    :param sigma_u: float
        Parameter sigma_u
    :return: float
        Estimated value of E(v|R)
    """
    theta1 = np.sqrt(np.exp( .0194975 )-1)
    theta2 = np.sqrt(np.exp(.0175016)-1)

    s = sigma_u * np.sqrt(1 + theta1**2 + theta2**2)
    
    omega1 = s * np.sqrt(1 + theta2**2) / theta1
    omega2 = s * np.sqrt(1 + theta1**2) / theta2
    
    lambda1 = (theta2 / theta1) * np.sqrt(1 + theta1**2 + theta2**2)
    lambda2 = (theta1 / theta2) * np.sqrt(1 + theta1**2 + theta2**2)
    
    phi = norm.pdf
    Phi = norm.cdf
    
    numerator = (2 * ((1 + theta2 ** 2) * sigma_u ** 2 / omega1) * phi(R / omega1) * Phi(lambda1 / omega1 * R) -
                 2 * theta1 ** 2 * sigma_u ** 2 / omega2 * phi(R / omega2) * Phi(lambda2 / omega2 * R))
    
    denominator = (Phi(R / omega1) - 2 * scipy_owens_t(R / omega1, lambda1) +
                   Phi(R / omega2) - 2 * scipy_owens_t(R / omega2, lambda2))
    
    return (theta1 ** 2 / (1 + theta1 ** 2 + theta2 ** 2)) * R + numerator / denominator



def calculate_function_science(row):
    R = row['abnret_d02']
    sigma_u = row['vol_e_new']
    sigma_u = sigma_u**(1/2)
    mktvalue = row['mktvalue']
    npatents = row['npatents']
    pi =  0.56 
    cond_exp = E_science_estimate(R,sigma_u)
    value=((cond_exp * mktvalue) / (1 - pi)) / (npatents)
    return value

def calculate_function_nonscience(row):
    R = row['abnret_d02']
    sigma_u = row['vol_e_new']
    sigma_u = sigma_u**(1/2)
    mktvalue = row['mktvalue']
    npatents = row['npatents']
    pi =  0.56 
    
    cond_exp = E_nonscience_estimate(R,sigma_u)
    value=((cond_exp * mktvalue) / (1 - pi)) / (npatents)
    return value
    

# Apply the function to each row and create a new column with the result
filtered_data = data[data['pat_allnonusa_invt_mix'] == 1]
filtered_data['allnonusa_new'] = filtered_data.apply(calculate_function_science, axis=1)

filtered_data['nononusa_new'] = filtered_data.apply(calculate_function_nonscience, axis=1)


# Convert the string column to datetime (without hours, minutes, and seconds)
filtered_data['date'] = pd.to_datetime(filtered_data['date'], format='%d%b%Y')

# If you want to convert it to a string data type explicitly
filtered_data['date'] = filtered_data['date'].astype(str)

#Save 
filename_save =os.path.join(wrd,r"Data\appendix\Multipatent_frompythonallnonusa_invt.dta")
print(filename_save)

filtered_data.to_stata(filename_save)