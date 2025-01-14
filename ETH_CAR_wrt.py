# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
import scipy.stats as stats
import os
import glob
from functools import reduce
import matplotlib.pyplot as plt
import pylab 
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import scipy.stats as st
import datetime
import pandas as pd
import numpy as np
import statsmodels.api as sm
from itertools import permutations
import pandas as pd

os.chdir("/Users/ruting/Library/Mobile Documents/com~apple~CloudDocs/ETH chronicles/code/Event_AR/")   #修改当前工作目录

# 1. data preparing

#  ETH return
crypto_df = pd.read_csv('Crypto_Price_20240407.csv')
crypto_df['Date'] = pd.to_datetime(crypto_df['Date']).dt.strftime('%Y-%m-%d')

ETH_price_df = crypto_df[['Date','ETH','BTC']]
ETH_price_df['Date'] = pd.to_datetime(ETH_price_df['Date'])

crypto_df.set_index('Date', inplace=True)
crypto_df.index = pd.DatetimeIndex(crypto_df.index)
crypto_df = crypto_df.drop(columns=['DOGE', 'RDD', 'BLK', 'FTC', 'ETC', 'VTC', 'BTG'])

# crypto_df['eth_return'] = np.log(crypto_df['ETH']).diff()
# ETH_price_df = crypto_df['eth_return']
# ETH_price_df = ETH_price_df.dropna()

# crypto_df = crypto_df.apply(pd.to_numeric, errors='coerce')
# crypto_df = np.log(crypto_df).diff()
# crypto_df.replace([np.inf, -np.inf], 0, inplace=True)
# crypto_df.replace([np.nan], 0, inplace=True)

# Crix return
crix_df = pd.read_csv('new_crix.csv')

crix_df['Date'] = pd.to_datetime(crix_df['date']).dt.strftime('%Y-%m-%d')
# crix_df.set_index('Date', inplace=True)
crix_df.drop(columns=['date'], inplace=True)

crix_df['Date'] = pd.to_datetime(crix_df['Date'])

full_date_range = pd.date_range(start=crix_df['Date'].min(), end=crix_df['Date'].max())
full_date_df = pd.DataFrame(full_date_range, columns=['Date'])
crix_df = pd.merge(full_date_df, crix_df, on='Date', how='left')
crix_df = pd.merge(crix_df, ETH_price_df, on='Date', how='left')
crix_df.set_index('Date', inplace=True)

crix_df = crix_df.loc['2017-09-07':'2024-04-07']
crix_df = crix_df.fillna(method='bfill')

crix_df['crix_return'] = np.log(crix_df['price']).diff()
crix_df['eth_return'] = np.log(crix_df['ETH']).diff()
crix_df['btc_return'] = np.log(crix_df['BTC']).diff()

crix_df = crix_df.rename(columns={'price':'crix_price'})
crix_df = crix_df[1:]

# replace zeros with btc returns
crix_df['crix_return'] = crix_df.apply(
    lambda row: row['btc_return'] if row['crix_return'] == 0 else row['crix_return'], axis=1
)
crix_df = crix_df.drop(columns=['BTC','btc_return'])

crypto_df['CRIX'] = crix_df['crix_return']
crypto_df = crypto_df.loc['2017-09-08':'2024-04-07']

crypto_df.replace([np.nan], 0, inplace=True)
crypto_df.replace([np.inf, -np.inf], 0, inplace=True)

crix_df = crix_df.sort_index()

event_dates = ['2017-10-16', '2019-02-28', '2019-12-08', '2020-01-02', '2020-10-14',
               '2020-12-01', '2021-04-15', '2021-08-05', '2021-10-27', '2021-12-09',
               '2022-06-30', '2022-09-06', '2022-09-15', '2023-04-12']

event_dates = pd.to_datetime(event_dates)

crix_df.index = pd.to_datetime(crix_df.index)

alphas = []
betas = []
pre_event_ar = []

for event_date in event_dates:
    # Extract the window of 60 days ( days before the event)
    # window_start = event_date - pd.Timedelta(days=37)  # 37 days to get 30 trading days
    window_start = event_date - pd.Timedelta(days=107)  # 107 days to get 100 trading days
    window_end = event_date - pd.Timedelta(days=7)
    
    window_df = crix_df.loc[window_start:window_end]
    
    if window_df[['crix_return', 'eth_return']].isnull().values.any():
        print(f"NaN values found for event {event_date}. Dropping NaNs.")
        window_df = window_df.dropna(subset=['crix_return', 'eth_return'])
    
    if np.isinf(window_df[['crix_return', 'eth_return']].values).any():
        print(f"Infinite values found for event {event_date}. Dropping infinities.")
        window_df = window_df[np.isfinite(window_df['crix_return']) & np.isfinite(window_df['eth_return'])]
    
    if window_df['crix_return'].nunique() == 1 or window_df['eth_return'].nunique() == 1:
        print(f"One of the columns has constant values for event {event_date}, which will cause issues in regression.")
        alphas.append(np.nan)
        betas.append(np.nan)
    else:
        X = window_df['crix_return'].values.reshape(-1, 1)
        y = window_df['eth_return'].values.reshape(-1, 1)

        X = sm.add_constant(X)
        
        model = sm.OLS(y, X)
        results = model.fit()

        alpha, beta = results.params
        alphas.append(alpha)
        betas.append(beta)
        
        pre_ar = window_df['eth_return'] - (beta * window_df['crix_return'].values + alpha)
        pre_event_ar.append(pre_ar)
        
results_df = pd.DataFrame({
    'event_date': event_dates,
    'alpha': alphas,
    'beta': betas
})

pre_event_ar = [df.reset_index(drop=True) for df in pre_event_ar]

pre_event_df = pd.concat(pre_event_ar, axis=1)
pre_event_df.columns = event_dates
std_ar = np.sum(pre_event_df ** 2) / (len(pre_event_df) -2)

def create(limit_before, limit_after, diff):
    length = int(((limit_before + limit_after) / diff) + 1)
    event_list = [-limit_before + i * diff for i in range(length)]
    return event_list

# Driver code
limit_before = 0
limit_after = 60
diff = 1
indices = create(limit_before, limit_after, diff)

expected_length = len(indices)

event_set = {}
for d in event_dates:
    idx = crix_df.index.get_loc(d)
    event = crix_df['eth_return'].iloc[(idx - limit_before): (idx + limit_after + 1)]
    if len(event) == expected_length:
        event.index = indices
        event_set[d] = event
    else:
        print(f"Length mismatch for event date {d}")

eth_return_df = pd.DataFrame(event_set)

crix_set = {}
for d in event_dates:
    idx = crix_df.index.get_loc(d)
    crix_data = crix_df['crix_return'].iloc[(idx - limit_before): (idx + limit_after + 1)]
    if len(crix_data) == expected_length:
        crix_data.index = indices
        crix_set[d] = crix_data
    else:
        print(f"Length mismatch for CRIX data on date {d}")

crix_return_df = pd.DataFrame(crix_set)

eth_er_df = crix_return_df.T.multiply(results_df['beta'].values, axis=0).T.add(results_df['alpha'].values, axis=1)


#  rewrite for AR calculation prob in eth_return_df

# eth_return_df = crix_return_df[]
eth_ar_df = eth_return_df.subtract(eth_er_df)
std_ar = np.sqrt((1 / (len(pre_event_df) - 2)) * np.sum((pre_event_df - pre_event_df.mean())**2, axis=0))

# CAR T-test
def calculate_car_t_test(eth_ar_df, std_ar):
    car_list = []
    std_ar_list = []
    t_stat_list = []
    p_values_list = []

    for column in eth_ar_df.columns:
        data = eth_ar_df[column].values
        car = np.sum(data)
        std = std_ar[column]
        n = len(data)
        
        # Calculate t-statistic
        # t_stat = car / (std / np.sqrt(n))
        t_stat = car / (std * np.sqrt(n))

        df = len(data) - 1
        
        # Calculate p-value
        p_value = 2 * (1 - stats.t.cdf(np.abs(t_stat), df))

        car_list.append(car)
        std_ar_list.append(std)
        t_stat_list.append(t_stat)
        p_values_list.append(p_value)

    combined_stats = pd.DataFrame({
        'event_date': eth_ar_df.columns.strftime('%Y-%m-%d'),
        'car': car_list,
        'std_ar': std_ar_list,
        't_stat': t_stat_list,
        'p_value': p_values_list,
    })

    # Round the results to 3 decimal places
    combined_stats[['car', 'std_ar', 't_stat', 'p_value']] = combined_stats[['car', 'std_ar', 't_stat', 'p_value']].applymap(lambda x: np.round(x * 1000) / 1000)

    return combined_stats

t_result = calculate_car_t_test(eth_ar_df, std_ar)
t_result

# Permutation part

limit_before_per = limit_before
limit_after_per = limit_after #30 20

diff = 1
indices = create(limit_before, limit_after, diff)

expected_length = len(indices)

event_set = {}
for d in event_dates:
    idx = crix_df.index.get_loc(d)
    event = crix_df['eth_return'].iloc[(idx - limit_before_per): (idx + limit_after_per + 1)]
    if len(event) == expected_length:
        event.index = indices
        event_set[d] = event
    else:
        print(f"Length mismatch for event date {d}")

eth_return_df = pd.DataFrame(event_set)

crix_set = {}
for d in event_dates:
    idx = crix_df.index.get_loc(d)
    crix_data = crix_df['crix_return'].iloc[(idx - limit_before_per): (idx + limit_after_per + 1)]
    if len(crix_data) == expected_length:
        crix_data.index = indices
        crix_set[d] = crix_data
    else:
        print(f"Length mismatch for CRIX data on date {d}")

crix_return_df = pd.DataFrame(crix_set)

eth_er_df = crix_return_df.T.multiply(results_df['beta'].values, axis=0).T.add(results_df['alpha'].values, axis=1)


#  rewrite for AR calculation prob in eth_return_df

# eth_return_df = crix_return_df[]
eth_ar_df = eth_return_df.subtract(eth_er_df)
std_ar = np.sqrt((1 / (len(pre_event_df) - 2)) * np.sum((pre_event_df - pre_event_df.mean())**2, axis=0))


def permutation_test(pre_event_df, event_df, std_ar, num_permutations):
    results = []
    summaries = []

    assert list(pre_event_df.columns) == list(event_df.columns), "Columns of pre_event_df and event_df must match"

    for event in pre_event_df.columns:
        pre_event_data = np.array(pre_event_df[event])
        event_data = np.array(event_df[event])

        actual_car = np.sum(event_data)
        
        std = std_ar[event]
        n = len(event_data)
        T_car_real = actual_car/(std * np.sqrt(n))

        # Combine pre-event and event data
        combined_df = np.concatenate([pre_event_data, event_data])
        
        # all_permutation = list(permutations(combined_df))
        # Temp_index = range((len(combined_df)))
        # num_permutations = 

        # Generate null distribution
        null_distribution = np.zeros(num_permutations)
        
        for  i in range(num_permutations):
            shuffled = np.random.permutation(combined_df)
            perm_event = shuffled[(len(shuffled)-n+1):]
         
            # pre-event
            shuffled_pre = shuffled[:(len(shuffled)-n)]
            
            shuffled_pre = shuffled_pre[~np.isnan(shuffled_pre)]
            perm_event = perm_event[~np.isnan(perm_event)]


            std_pre = np.sqrt((1 / (len(shuffled_pre) - 2)) * np.sum((shuffled_pre - shuffled_pre.mean())**2, axis=0))
            T_car_per =  np.sum(perm_event) / (std_pre * np.sqrt(n))
            
            null_distribution[i] = T_car_per
            

        # Calculate p-value
        p_value = np.mean(null_distribution >= T_car_real)
     
        # Collect results for this event
        results.append(pd.DataFrame({
            'Event': event,
            'Permutation': np.arange(1, num_permutations + 1),
            'Permuted CAR': null_distribution
        }))
        summaries.append(pd.DataFrame({
            'Event': [event],
            'Actual CAR': [actual_car],
            'P-value': [p_value]
        }))

    results_df = pd.concat(results, ignore_index=True)
    summary_df = pd.concat(summaries, ignore_index=True)

    return results_df, summary_df


results_df_per, summary_df_per = permutation_test(pre_event_df, eth_ar_df,std_ar,num_permutations = 1000)
summary_df_per

output_dir = 'CAR'
os.makedirs(output_dir, exist_ok=True)

# output to excel
output_filename = output_dir+'/CAR_Result_before_'+ str(limit_before)+'_after_'+str(limit_after)+'.xlsx'

with pd.ExcelWriter(output_filename) as writer:
    t_result.to_excel(writer, sheet_name='Ordinary', index=False)
    summary_df_per.to_excel(writer, sheet_name='Permutation', index=False)
    

