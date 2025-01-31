from utils import save_data

def add_user(users, user):
    users['usuarios'].append(user)
    save_data(users, 'data/usuarios.json')

def authenticate_user(users, numdoc, codun):
    for user in users['usuarios']:
        if user['numdoc'] == numdoc and user['codun'] == codun:
            return user
    return None

def check_permissions(user, action):
    if user['carg'] == 'gerente':
        return True
    elif user['carg'] == 'cajero' and action in ['read']:
        return True
    return False
