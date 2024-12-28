# feature engineering
import os
import pandas as pd
from pathlib import Path
import sys
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import StandardScaler
from sklearn.feature_extraction import DictVectorizer

current_dir = Path(__file__).resolve().parent
root_dir = current_dir.parent.parent
sys.path.append(str(root_dir))

from src.data.load_data import load_data
from src.features.data_preprocessing import preprocess

def feature_engineering(df):
    """
    Processes the given DataFrame by splitting it into training, validation, and test sets,
    scaling numerical columns, and encoding categorical and numerical columns.

    :param df: pandas DataFrame to process
    :return: df_train_full, df_test, df_train, df_val, y_train, y_val, X_train, X_val
    """
    # Split the dataset into full training and test sets
    df_train_full, df_test = train_test_split(df, test_size=0.2, random_state=1)
    
    # Further split the training set into train and validation sets
    df_train, df_val = train_test_split(df_train_full, test_size=0.33, random_state=11)
    
    # Extract the target variable
    target_column = 'history_of_mental_illness'
    y_train = df_train[target_column].values
    y_val = df_val[target_column].values
    
    # Drop the target column from the training and validation datasets
    df_train = df_train.drop(columns=[target_column])
    df_val = df_val.drop(columns=[target_column])
    
    # Scale numerical columns
    numerical = ['income']
    sc = StandardScaler()
    df_train[numerical] = sc.fit_transform(df_train[numerical])
    df_val[numerical] = sc.transform(df_val[numerical])
    
    # Define categorical columns
    categorical = [
        'education_level', 'employment_status',
         'dietary_habits', 'sleep_patterns'
    ]
    
    # Encode categorical and numerical columns using DictVectorizer
    train_dict = df_train[categorical + numerical].to_dict(orient='records')
    val_dict = df_val[categorical + numerical].to_dict(orient='records')
    
    dv = DictVectorizer(sparse=False)
    dv.fit(train_dict)
    
    X_train = dv.transform(train_dict)
    X_val = dv.transform(val_dict)
    
    return df_train_full, df_test, df_train, df_val, y_train, y_val, X_train, X_val


if __name__ == "__main__":
    data = load_data()
    columns_to_drop = ['Unnamed: 0', 'agecat', 'incomecat','smoking2cat','alcohol.cat']
    data2 = preprocess(data,columns_to_drop)
    df_train_full, df_test, df_train, df_val, y_train, y_val, X_train, X_val = feature_engineering(data2)

    print(y_val)




