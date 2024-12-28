# hyperparameter_tuning.py
import sys
import os
from pathlib import Path

current_dir = Path(__file__).resolve().parent
root_dir = current_dir.parent.parent
sys.path.append(str(root_dir))

from src.data.load_data import load_data # from data_loading.py file
from src.features.data_preprocessing import preprocess # from data_preprocessing.py file
from src.models.model_training import train
from src.features.model_evaluation import evaluate_model
from feature_engineering import feature_engineering
from sklearn.model_selection import GridSearchCV
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score

def tune_hyperparameters(X_train, y_train):
    """
    Perform hyperparameter tuning for LogisticRegression using GridSearchCV.
    
    :param X_train: Training feature matrix
    :param y_train: Training target values
    :return: Dictionary of the best hyperparameters
    """
    param_grid = {
        # Hyperparameters to tune:
        'C': [0.01, 0.1, 1, 10, 100],           # Regularization strength
        'penalty': ['l1', 'l2'],                # Type of regularization
        'class_weight': [None, 'balanced'],     # Class weight
        'solver': ['liblinear', 'saga']         # Solvers supporting l1 and l2 penalties
    }

    # Initialize the grid search
    grid_search = GridSearchCV(
        LogisticRegression(random_state=42),
        param_grid,
        cv=3,              # 3-fold cross-validation
        scoring='roc_auc', # Use AUC as the scoring metric
        verbose=1,
        n_jobs=-1          # Use all available processors
    )
    
    # Fit the grid search
    grid_search.fit(X_train, y_train)
    
    return grid_search.best_params_

if __name__ == "__main__":
    data = load_data()
    columns_to_drop = ['Unnamed: 0', 'agecat', 'incomecat','smoking2cat','alcohol.cat']
    data2 = preprocess(data,columns_to_drop)

    df_train_full, df_test, df_train, df_val, y_train, y_val, X_train, X_val = feature_engineering(data2)
#    Tune hyperparameters
    best_params = tune_hyperparameters(X_train, y_train)

    print(f"The best hyperparameters are {best_params}")
