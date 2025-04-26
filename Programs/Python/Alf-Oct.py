# -*- coding: utf-8 -*-
"""
Name: Conversor de Alfabetico a Octal
Description: Programa Diceñado Para Convertir palabras, Oraciones, Textos E.t.c. A Octal
Author: Andres Camilo Laguna Bernal
Date:
Terminado: Si
"""
import pickle

# Crear diccionario de traducción alfabético → octal
alfabeto = {chr(i): oct(i - 65)[2:] for i in range(65, 91)}  # Mayúsculas A-Z
alfabeto.update({chr(i): oct(i - 97)[2:] for i in range(97, 123)})  # Minúsculas a-z

# Guardar en un archivo .dat
with open("diccionario.dat", "wb") as f:
    pickle.dump(alfabeto, f)

# Función para cargar el diccionario
def cargar_diccionario():
    with open("diccionario.dat", "rb") as f:
        return pickle.load(f)

# Función para traducir texto a octal
def traducir_a_octal(texto):
    diccionario = cargar_diccionario()
    return " ".join(diccionario.get(letra, "?") for letra in texto)

# Ejemplo de uso
if __name__ == "__main__":
    palabra = input("Ingrese un texto: ")
    print("Traducción en octal:", traducir_a_octal(palabra))
