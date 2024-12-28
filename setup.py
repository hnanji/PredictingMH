# setup.py
import os

folders = [
    "data",
    "data/processed",
    "data/raw",
    "data/external",
    "data/transformed",
    "docs",
    "models",
    "notebooks",
    "references",
    "reports",
    "reports/figures",
    "src",
    "src/data",
    "src/features",
    "src/models",
    "src/visualization",
]


for folder in folders:
    os.makedirs(folder, exist_ok=True)
    with open(os.path.join(folder, ".gitkeep"), "w") as f:
        pass

files = [
    "src/__init__.py",
    "src/data/make_dataset.py",
    "src/features/build_features.py",
    "src/models/train_model.py",
    "src/models/predict_model.py",
    "src/visualization/visualize.py",
    "requirements.txt"
]

for file in files:
    with open(file, "w") as f:
        pass