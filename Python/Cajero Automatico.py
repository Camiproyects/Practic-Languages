import json
import os 

def Cargar_Usuarios():
    """Carga las tareas desde un archivo JSON."""
    if os.path.exists("Cajero Automatico.json"):
        with open("Cajero Automatico.json", "r") as archivo:
            return json.load(archivo)
    return []

def Selecion_De_Usuario():
    """Se Da A Escoger El Tipo De Usuario"""
    print("\n--- Gestor de Usuarios ---")
    print("2. Acceder Como Cliente")
    print("1. Acceder Como Administrador")
    print("3. Crea Un Usuario Como Cliente")

def Acceso_Administrador():
    """Se Realiza Inicio De Secion"""
    Correo_A = input("Escribe Tu Correo: ")
    Pass_A = input("Pon Tu Contraseña: ")

def Acceso_Cliente():
    """Se Realiza Inicio De Secion"""
    Correo_C = input("Escribe Tu Correo: ")
    Pass_C = input("Pon Tu Contraseña: ")

    open("Cajero Automatico.json")

def Crear_Usuario(Usuarios):
    """Se Crea Un Usuario (Cliente)"""
    Nombre = input("Escribe Tu Nombre: ")
    Correo_C = input("Escribe Tu Correo: ")
    Pass_C = input("Escribe Tu Contraseña: ")
    Saldo  = 0
    Crear_Usuario = {
        "Nombre" : Nombre,
        "Correo_C" : Correo_C,
        "Pass_C" : Pass_C,
        "Saldo" : Saldo,
    }
    Usuarios.append(Crear_Usuario)
    print("Uauario Creado Con Exito")
    print("Bienvenido ",Nombre)


def main():
    Usuarios = Cargar_Usuarios()
    """Cabeza Principal Del Programa"""
    while True:
        Selecion_De_Usuario()
        opcion = input("Elige una opción: ")
        if opcion == "1":
            Acceso_Cliente()
        elif opcion == "2":
            Acceso_Administrador()
        elif opcion == "3":
            Crear_Usuario(Usuarios)


if __name__ == "__main__":
    main()