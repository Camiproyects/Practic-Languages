# -*- coding: utf-8 -*-
"""
Name: Administrador de Tareas
Description: Programa Diceñado Para La Administracion De Tareas (Datos: Titulo, Descripcion, Fecha Limite, Completada)
Author: Andres Camilo Laguna Bernal
Date:
Terminado: Si
"""
import json	
import os

def cargar_tareas():
    """Carga las tareas desde un archivo JSON."""
    if os.path.exists("Tareas.json"):
        with open("Tareas.json", "r") as archivo:
            return json.load(archivo)
    return []

def guardar_tareas(tareas):
    """Guarda las tareas en un archivo JSON."""
    with open("Tareas.json", "w") as archivo:
        json.dump(tareas, archivo, indent=4)

def mostrar_tareas(tareas):
    """Muestra todas las tareas con su estado."""
    if not tareas:
        print("No hay tareas registradas.")
        return

    for i, tarea in enumerate(tareas, start=1):
        estado = "Completada" if tarea["completada"] else "Pendiente"
        print(f"{i}. {tarea['titulo']} - {estado} (Fecha límite: {tarea['fecha']})")

def agregar_tarea(tareas):
    """Agrega una nueva tarea a la lista."""
    titulo = input("Título de la tarea: ")
    descripcion = input("Descripción: ")
    fecha = input("Fecha límite (YYYY-MM-DD): ")
    tarea = {
        "titulo": titulo,
        "descripcion": descripcion,
        "fecha": fecha,
        "completada": False
    }
    tareas.append(tarea)
    print("Tarea agregada con éxito.")

def editar_tarea(tareas):
    """Edita una tarea existente."""
    mostrar_tareas(tareas)
    indice = int(input("Número de la tarea a editar: ")) - 1
    if 0 <= indice < len(tareas):
        tareas[indice]["titulo"] = input("Nuevo título: ") or tareas[indice]["titulo"]
        tareas[indice]["descripcion"] = input("Nueva descripción: ") or tareas[indice]["descripcion"]
        tareas[indice]["fecha"] = input("Nueva fecha límite (YYYY-MM-DD): ") or tareas[indice]["fecha"]
        print("Tarea editada con éxito.")
    else:
        print("Índice no válido.")

def completar_tarea(tareas):
    """Marca una tarea como completada."""
    mostrar_tareas(tareas)
    indice = int(input("Número de la tarea a completar: ")) - 1
    if 0 <= indice < len(tareas):
        tareas[indice]["completada"] = True
        print("Tarea marcada como completada.")
    else:
        print("Índice no válido.")

def eliminar_tarea(tareas):
    """Elimina una tarea de la lista."""
    mostrar_tareas(tareas)
    indice = int(input("Número de la tarea a eliminar: ")) - 1
    if 0 <= indice < len(tareas):
        tareas.pop(indice)
        print("Tarea eliminada con éxito.")
    else:
        print("Índice no válido.")

def mostrar_menu():
    """Muestra el menú de opciones."""
    print("\n--- Gestor de Tareas ---")
    print("1. Ver tareas")
    print("2. Agregar tarea")
    print("3. Editar tarea")
    print("4. Marcar tarea como completada")
    print("5. Eliminar tarea")
    print("6. Salir")

def main():
    """Función principal del programa."""
    tareas = cargar_tareas()
    while True:
        mostrar_menu()
        opcion = input("Elige una opción: ")
        if opcion == "1":
            mostrar_tareas(tareas)
        elif opcion == "2":
            agregar_tarea(tareas)
        elif opcion == "3":
            editar_tarea(tareas)
        elif opcion == "4":
            completar_tarea(tareas)
        elif opcion == "5":
            eliminar_tarea(tareas)
        elif opcion == "6":
            guardar_tareas(tareas)
            print("¡Hasta luego!")
            break
        else:
            print("Opción no válida. Intenta de nuevo.")

if __name__ == "__main__":
    main()
