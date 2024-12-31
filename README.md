Project Name: Predicting Mental Health Illness

Objective:
The purpose of this project is to analyze patient-level data, perform exploratory data analysis, and develop a machine learning algorithm to predict patients with mental health illnesses.

Methods Used:

Inferential Statistics
Machine Learning
Data Visualization
Predictive Modeling
Technologies:

R
Python
Project Description:
The dataset used for this project is the Depression Dataset obtained from Kaggle.

Data Preparation: R was used to clean, preprocess, and prepare the data for analysis.
Visualization: The ggplot2 package was utilized to visualize the data and uncover interesting patterns.
Machine Learning: Multiple classification algorithms were implemented to predict patients with mental health illnesses. The best-performing model was selected using Receiver Operating Characteristic (ROC) curves and the Area Under the Curve (AUC) metric.
Order of Execution:

Use the requirements.txt file or the environment.yml file to replicate the environment with all dependencies.
Download and install R and RStudio.
Run the mentalhealth.R script (located in the notebooks folder). This script performs the following tasks:
Loads the raw data
Cleans and preprocesses the data
Produces and saves all visualizations in the reports folder
Prepares the data for machine learning tasks and saves it in the data/processed folder
Use the Jupyter Notebook titled PredictingMentalHealth3 for machine learning experimentation.
The model_serialization.py file automates the entire end-to-end process, including:
Data loading
Preprocessing
Feature transformation
Model training and evaluation
Hyperparameter tuning
Saving the final trained model
