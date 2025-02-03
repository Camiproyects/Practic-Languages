import json
from datetime import datetime

def cargar_articulos():
    try:
        with open('../data/articulos.json', 'r') as f:
            return json.load(f)['articulos']
    except FileNotFoundError:
        return []

def guardar_articulos(articulos):
    with open('../data/articulos.json', 'w') as f:
        json.dump({"articulos": articulos}, f, indent=4)

def validar_fecha_expiracion(fecha):
    if fecha:
        return datetime.strptime(fecha, "%Y-%m-%d") > datetime.now()
    return True
