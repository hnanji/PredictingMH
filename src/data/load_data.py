
# data load
import os
import pandas as pd
from pathlib import Path
import os
import pandas as pd

# create the function to load the data
def load_data():
    current_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.abspath(os.path.join(current_dir, '..'))
    file_path = os.path.join(current_dir, '..', '..', 'data', 'processed', 'analysis_data_set.csv')
    file_path = os.path.abspath(file_path)
    data = pd.read_csv(file_path)
    return data

if __name__ == "__main__":
    data = load_data()
    print(data.head(5))


