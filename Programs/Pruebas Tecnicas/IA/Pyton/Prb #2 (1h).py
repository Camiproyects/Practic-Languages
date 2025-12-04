"""
TERMINADO:Si.

1.la prueba esta generada por IA.
2.tiene una duracion de 45 minutos.
3.puedes resolver todo en pyton 3.
4.no uses librerias externas solo estructuras basicas.
5.Problema general:
    Desarrollar un sistema de gestión de órdenes de servicio.
    Cada orden tiene: id, descripción, estado, fecha de apertura y fecha de cierre.

    Requerimientos funcionales (20 puntos):
        1. (1 pt) Clase OrdenServicio con atributos básicos.
        2. (2 pts) Crear nuevas órdenes.
        3. (2 pts) Cambiar estado con validación y cierre automático.
        4. (1 pt) Manejo de errores de estado inválido.
        5. (2 pts) Listar órdenes filtradas por estado.(recibido, en revicion, en trabajo,trabajo listo, no se pudo realizar el trabajo, entregado)
        6. (2 pts) Calcular tiempo de resolución.
        7. (2 pts) Generar resumen estadístico.
        8. (1 pt) Manejo correcto de fechas con datetime.
        9. (2 pts) Código limpio, estructurado, con funciones o clases.
        10. (5 bonus pts) Persistencia con JSON o CSV.
"""

from datetime import datetime
import os 
import json

def New_Ord():
    if os.path.exists("Programs\Pruebas Tecnicas\IA\Pyton\datos.json"):
        with open("Programs\Pruebas Tecnicas\IA\Pyton\datos.json", "r", encoding="utf-8") as f:
            ordenes = json.load(f)
    else:
        ordenes = []
    
    if ordenes:
        nuevo_id = max(orden["id"] for orden in ordenes) + 1
    else:
        nuevo_id = 1
    
    descripción = str(input("Describe la orden: "))
    
    fecha_de_apertura = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    nueva_orden = {
        "id": nuevo_id,
        "descripción" : descripción,
        "estado": "recibido",
        "fecha_de_apertura" : fecha_de_apertura,
        "fecha_de_cierre" : None
    }
    print(f"ID: {nueva_orden["id"]}\ndescripción: {nueva_orden["descripción"]}\nEstado: {nueva_orden["estado"]}\napertura: {nueva_orden["fecha_de_apertura"]}\ncierre: {nueva_orden["fecha_de_cierre"]}")

    ordenes.append(nueva_orden)
    
    with open("Programs\Pruebas Tecnicas\IA\Pyton\datos.json", "w", encoding="utf-8") as f:
        json.dump(ordenes, f, indent=4, ensure_ascii=False)


def WF_State():
    with open("Programs\Pruebas Tecnicas\IA\Pyton\datos.json", "r", encoding="utf-8") as f:
        ordenes = json.load(f)

    id_bus = int(input("ingresa el id_ de la orden"))

    found = False
    for orden in ordenes:
        if orden["id"] == id_bus:
            found = True
            print(f"Estado actual{orden["estado"]}")
            print("1: en revicion\n2: en trabajo\n3: trabajo listo\n4:no se pudo realizar el trabajo\n5:entregado")
            option = int(input("Poravor digitar el nuevo estado: "))
            estados = {
                1: "en revicion",
                2: "en trabajo",
                3: "trabajo listo",
                4: "no se pudo realizar el trabajo",
                5: "entregado"
            }
            if option in estados:
                orden["estado"] = estados["option"]
                if option == 5:
                    orden["fecha_de_cierre"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                else:
                    print("No es una opcion valida")
                    break
                if not found:
                    print("Nuemro de orden no encontrada")
        else:
            print("nuemro de orden no encontrada")
    
    with open("Programs\Pruebas Tecnicas\IA\Pyton\datos.json", "w", encoding="utf-8") as f:
        json.dump(ordenes, f, indent=4, ensure_ascii=False)
    print(f"Orden con ID_ {id_bus} actualizada a el estado {orden["estado"]}")


def Lis_Est():
    with open("Programs\Pruebas Tecnicas\IA\Pyton\datos.json", "r", encoding="utf-8") as f:
        ordenes = json.load(f)
    
    for orden in ordenes:
        if orden["estado"] != "entregado":
            print(f"ID:{orden["id"]}\ndescripción: {orden["descripción"]}\nEstado {orden["estado"]}\napertura{orden["fecha_de_apertura"]}\ncierre{orden["fecha_de_cierre"]}")
    


def menu():
    while True:
        print("-- Seleciona el tipo de operacion que deseas realizar --")
        print("-- 1: Crear una nueva orden de servicio               --")
        print("-- 2: Cambiar estado de validacion de un servicio     --")
        print("-- 3: Listar ordenes por estado                       --")
        print("-- 4: listar ordenes por estado                       --")

        option = int(input("Porfavor digita tu opción: "))

        if   option == 1:
            New_Ord()
        elif option == 2:
            WF_State()
        elif option == 3:
            Lis_Est()
        else:
            print("--            Gracias =P                          --")

    

if __name__ == "__main__":
    menu()
