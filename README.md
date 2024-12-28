Project Name: Predicting Mental Health Illness

Objective: The propose of this project is to use patient level data, perform exploratory data analysis and build a machine learning algorithm the predicts patients with mental health illness.

Methods used:
Inferential Statistics
Machine Learning
Data Visualization
Predictive Modeling
Technologies:
R/Python

Project Description:
The data source: Depression data set from https://www.kaggle.com/datasets/anthonytherrien/depression-dataset/data.
R was used to clean preprocess and prepare the data for analysis, ggplot2 was used to visualise the data to find interesting patterns. For machine learning several classification algorithms were used to predict patients with mental health and the best performing one selecetd using Receiver operating curves (Area under the curve)

Order of execution
Use either the requirement.txt file or the environment.yml file to replicate the environment with all its dependencies.
Download and install R / Rstudio. Run the mentalhealth.R script( found in notebook folder) which will load the raw data, clean and preprocess it and produce all visuals. All the visuals are saved under the folder ‘reports’. This script also prepares the data and saved it under data/preprocess for machine learning task
Using jupyter notebook titled PredictingMentalHealth3 for all experimentation for machine learning.The model_seralisation.py file automates all the end to end process of data load, preprocessing, feature transformation, model training evaluation, hyoperparameter tuning and the final model saved.
