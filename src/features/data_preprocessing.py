# data_preprocessing.py
import sys
import pandas as pd
from pathlib import Path
import os
import pandas as pd

current_dir = Path(__file__).resolve().parent
root_dir = current_dir.parent.parent
sys.path.append(str(root_dir))

from src.data.load_data import load_data

def preprocess(data,columns_to_drop):

    columns_to_drop = ['Unnamed: 0', 'agecat', 'incomecat','smoking2cat','alcohol.cat',
                               'marital_status', 'number_of_children',
       'smoking_status', 'physical_activity_level',
        'alcohol_consumption','history_of_substance_abuse',
       'family_history_of_depression', 'chronic_medical_conditions','age']
    data = data.drop(columns=columns_to_drop)
    return data


if __name__ == "__main__":
    data = load_data()
    columns_to_drop = ['Unnamed: 0', 'agecat', 'incomecat','smoking2cat','alcohol.cat']
    data2 = preprocess(data,columns_to_drop)
    print(data2.columns)

   