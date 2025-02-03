import json
import datetime

def load_data(file_path):
    with open(file_path, 'r') as file:
        return json.load(file)

def save_data(data, file_path):
    with open(file_path, 'w') as file:
        json.dump(data, file, indent=4)

def validate_expiry(date_str):
    try:
        expiry_date = datetime.datetime.strptime(date_str, '%d-%m-%Y')
        return expiry_date >= datetime.datetime.now()
    except ValueError:
        return False
