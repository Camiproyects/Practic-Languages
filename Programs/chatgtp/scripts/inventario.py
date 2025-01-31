import json
import os

DATA_DIR = os.path.join(os.path.dirname(__file__), "../data")
ART_FILE = os.path.join(DATA_DIR, "articulos.json")
USR_FILE = os.path.join(DATA_DIR, "usuarios.json")

def cargar_datos(archivo):
    if os.path.exists(archivo):
        with open(archivo, "r", encoding="utf-8") as f:
            return json.load(f)
    return []

def guardar_datos(archivo, datos):
    with open(archivo, "w", encoding="utf-8") as f:
        json.dump(datos, f, indent=4)

def agregar_articulo(codbar, codint, des, precom, preven, stk, cant, fecha=None):
    articulos = cargar_datos(ART_FILE)
    articulos.append({"codbar": codbar, "codint": codint, "des": des, "precom": precom, "preven": preven, "stk": stk, "cant": cant, "fecha": fecha})
    guardar_datos(ART_FILE, articulos)

def eliminar_articulo(codint):
    articulos = cargar_datos(ART_FILE)
    articulos = [art for art in articulos if art["codint"] != codint]
    guardar_datos(ART_FILE, articulos)

def obtener_alertas_stock(limite=5):
    articulos = cargar_datos(ART_FILE)
    return [art for art in articulos if art["stk"] <= limite]

def agregar_usuario(nom, tidoc, numdoc, cupag, numcon, cro, carg, det, codun):
    if carg not in ["gerente", "cajero"]:
        raise ValueError("Cargo no válido")
    usuarios = cargar_datos(USR_FILE)
    usuarios.append({"nom": nom, "tidoc": tidoc, "numdoc": numdoc, "cupag": cupag, "numcon": numcon, "cro": cro, "carg": carg, "det": det, "codun": codun})
    guardar_datos(USR_FILE, usuarios)

def validar_credenciales(numdoc, codun):
    usuarios = cargar_datos(USR_FILE)
    return any(u["numdoc"] == numdoc and u["codun"] == codun for u in usuarios)

if __name__ == "__main__":
    print("Módulo de gestión de inventario")
