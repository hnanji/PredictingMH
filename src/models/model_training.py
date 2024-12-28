# model_training.py
import sys
import os
from pathlib import Path

current_dir = Path(__file__).resolve().parent
root_dir = current_dir.parent.parent
sys.path.append(str(root_dir))

from src.data.load_data import load_data
from src.features.data_preprocessing import preprocess
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score
from src.features.feature_engineering import feature_engineering
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score
from sklearn.preprocessing import StandardScaler
from sklearn.feature_extraction import DictVectorizer

def train(X_train, y_train):
    """
    Trains a logistic regression model, preprocesses validation data, makes predictions,
    computes the AUC score, and returns the trained model and AUC score.

    :param X_train: Feature matrix for training
    :param y_train: Target values for training
    :param df_val: Validation DataFrame (features before transformation)
    :param y_val: Target values for validation
    :return: Trained logistic regression model, AUC score
    """
    # Train logistic regression model
    model = LogisticRegression(solver='liblinear', random_state=1)
    model.fit(X_train, y_train)
    print("Logistic Regression Model instatntiated")
    return model

if __name__ == "__main__":
    data = load_data()
    columns_to_drop = ['Unnamed: 0', 'agecat', 'incomecat','smoking2cat','alcohol.cat']
    data2 = preprocess(data,columns_to_drop)
    df_train_full, df_test, df_train, df_val, y_train, y_val, X_train, X_val = feature_engineering(data2)
    model = train(X_train, y_train)

    print(model)


