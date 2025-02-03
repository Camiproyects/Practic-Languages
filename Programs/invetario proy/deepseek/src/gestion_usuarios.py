import json

def cargar_usuarios():
    try:
        with open('../data/usuarios.json', 'r') as f:
            return json.load(f)['usuarios']
    except FileNotFoundError:
        return []

def guardar_usuarios(usuarios):
    with open('../data/usuarios.json', 'w') as f:
        json.dump({"usuarios": usuarios}, f, indent=4)

def validar_credenciales_gerente(numdoc, codun, usuarios):
    for user in usuarios:
        if user['numdoc'] == numdoc and user['codun'] == codun and user['carg'] == 'gerente':
            return True
    return False
