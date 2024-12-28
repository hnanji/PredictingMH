import sys
import os
from pathlib import Path
import joblib
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
from src.data.load_data import load_data
from src.features.data_preprocessing import preprocess
from src.features.feature_engineering import feature_engineering
from src.models.model_training import train


current_dir = Path(__file__).resolve().parent
root_dir = current_dir.parent.parent 
sys.path.append(str(root_dir))

# Function to serialize and save the model
def serialize_model(model, filename):
    """
    Save the model to the specified file path.
    :param model: Trained model
    :param filename: Path to save the model
    """
    joblib.dump(model, filename)
    print(f"Model serialized and saved as '{filename}'")

if __name__ == "__main__":
    # Load and preprocess data
    data = load_data()
    columns_to_drop = ['Unnamed: 0', 'agecat', 'incomecat', 'smoking2cat', 'alcohol.cat']
    preprocessed_data = preprocess(data, columns_to_drop)
    
    # Feature engineering
    df_train_full, df_test, df_train, df_val, y_train, y_val, X_train, X_val = feature_engineering(preprocessed_data)
    
    # Train the base model
    trained_model = train(X_train, y_train)
    
    # Define the hyperparameter grid for tuning
    param_grid = {
        'C': [0.01, 0.1, 1, 10, 100],      # Regularization strength
        'penalty': ['l2'],                 # Type of regularization
        'class_weight': ['balanced'],      # Class weight
        'solver': ['liblinear']            # Solver
    }
    
    # Initialize GridSearchCV for hyperparameter tuning
    grid_search = GridSearchCV(
        LogisticRegression(random_state=42),
        param_grid,
        cv=3,              # 3-fold cross-validation
        scoring='roc_auc', # Use AUC as the scoring metric
        verbose=1,
        n_jobs=-1          # Use all available processors
    )
    
    # Perform the grid search on the training data
    grid_search.fit(X_train, y_train)
    
    # Get the best-tuned model
    tuned_model = grid_search.best_estimator_
    print("Best parameters:", grid_search.best_params_)

    current_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.abspath(os.path.join(current_dir, '..'))
    model_save_path = os.path.join(current_dir, 'models', 'best_model.pkl')
    serialize_model(tuned_model, model_save_path)
    
