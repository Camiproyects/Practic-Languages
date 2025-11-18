"""
TERMINADO: NO.

1. La prueba está generada por IA.
2. Tiene una duración de 1 hora y 30 minutos.
3. Puedes resolver todo en Python 3.
4. No uses librerías externas, solo estructuras básicas.
5. PROBLEMA GENERAL:
    Una empresa de envíos llamada LogiTrack necesita un sistema para gestionar paquetes
    que sus clientes envían a distintas ciudades. El sistema debe registrar paquetes,
    calcular costos según peso y distancia, y generar reportes.

6. REQUERIMIENTOS FUNCIONALES (20 PUNTOS):
    1. (2 pts) Clase principal Paquete:
       - Atributos: id, cliente, peso_kg, ciudad_destino, distancia_km, estado ("en tránsito", "entregado").

    2. (2 pts) Clase SistemaEnvios:
       - Contiene una lista de paquetes y métodos para gestionarlos.

    3. (2 pts) Método registrar_paquete():
       - Solicita datos del usuario y los guarda con un ID incremental.

    4. (2 pts) Método calcular_costo(paquete):
       - Fórmula: costo = (peso_kg * 1000) + (distancia_km * 5).

    5. (1 pt) Método actualizar_estado(id_paquete, nuevo_estado):
       - Valida el estado ("en tránsito" o "entregado").

    6. (1 pt) Método listar_paquetes_por_estado():
       - Muestra todos los paquetes con el estado seleccionado.

    7. (2 pts) Método buscar_paquete():
       - Permite buscar por nombre de cliente o ciudad destino (sin importar mayúsculas).

    8. (3 pts) Método generar_reporte():
       - Muestra: total de paquetes, entregados, pendientes, peso total y promedio de costo.

    9. (3 pts) Persistencia con JSON:
       - Guardar los datos en "paquetes.json" y poder cargarlos automáticamente.

    10. (2 pts) Código limpio y modular:
        - Uso de clases, funciones y comentarios adecuados.

7. CASOS DE PRUEBA SUGERIDOS:
    - Registrar paquete → Calcular costo → Mostrar datos.
    - Actualizar estado → Confirmar cambio.
    - Generar reporte → Mostrar totales y promedios.

8. ESTRUCTURA SUGERIDA:
    - Clase Paquete
    - Clase SistemaEnvios
    - Funciones principales
    - Menú de opciones
"""

class Paquete:
    def __init__(self, id, cliente, peso_kg, ciudad_destino, distancia_km):
        self.id = id
        self.cliente = cliente
        self.peso_kg = peso_kg
        self.ciudad_destino = ciudad_destino
        self.distancia_km = distancia_km
        self.estado = "en tránsito"

def calcular_costo(self):
    return (self.peso_kg * 1000) + (self.distancia_km * 5)

class SistemaEnvios:
    def __init__(self):
        self.paquetes = []
    def registrar_paquete(self):
        id = len(self.paquetes) + 1
        cliente = input("Ingrese el nombre del cliente: ")
        peso_kg = float(input("Ingrese el peso en kg: "))
        ciudad_destino = input("Ingrese la ciudad destino: ")
        distancia_km = float(input("Ingrese la distancia en km: "))
        paquete = Paquete(id, cliente, peso_kg, ciudad_destino, distancia_km)
        self.paquetes.append(paquete)
        print(f"Paquete registrado con ID: {id}")
    def actualizar_estado(self, id_paquete, nuevo_estado):
        for paquete in self.paquetes:
            if paquete.id == id_paquete:
                if nuevo_estado in ["en tránsito", "entregado"]:
                    paquete.estado = nuevo_estado
                    print(f"Estado del paquete ID {id_paquete} actualizado a {nuevo_estado}")
                else:
                    print("Estado inválido. Use 'en tránsito' o 'entregado'.")
                return
        print("Paquete no encontrado.")
    def listar_paquetes_por_estado(self, estado):
        for paquete in self.paquetes:
            if paquete.estado == estado:
                print(f"ID: {paquete.id}, Cliente: {paquete.cliente}, Ciudad: {paquete.ciudad_destino}, Estado: {paquete.estado}")
    def buscar_paquete(self, criterio):
        criterio = criterio.lower()
        for paquete in self.paquetes:
            if criterio in paquete.cliente.lower() or criterio in paquete.ciudad_destino.lower():
                print(f"ID: {paquete.id}, Cliente: {paquete.cliente}, Ciudad: {paquete.ciudad_destino}, Estado: {paquete.estado}")
    def generar_reporte(self):
        total_paquetes = len(self.paquetes)
        entregados = sum(1 for p in self.paquetes if p.estado == "entregado")
        pendientes = total_paquetes - entregados
        peso_total = sum(p.peso_kg for p in self.paquetes)
        costo_total = sum(p.calcular_costo() for p in self.paquetes)
        costo_promedio = costo_total / total_paquetes if total_paquetes > 0 else 0
        print(f"Total de paquetes: {total_paquetes}")
        print(f"Entregados: {entregados}")
        print(f"Pendientes: {pendientes}")
        print(f"Peso total: {peso_total} kg")
        print(f"Costo promedio: ${costo_promedio:.2f}")

"""RETO ADICIONAL (10 PUNTOS):
- Implementar persistencia de datos utilizando JSON.
- Almacenar la información de los paquetes en un archivo "paquetes.json".
- Cargar automáticamente los datos al iniciar el sistema.
"""
import json
import os
def guardar_datos(sistema):
    datos = []
    for paquete in sistema.paquetes:
        datos.append({
            "id": paquete.id,
            "cliente": paquete.cliente,
            "peso_kg": paquete.peso_kg,
            "ciudad_destino": paquete.ciudad_destino,
            "distancia_km": paquete.distancia_km,
            "estado": paquete.estado
        })
    with open("paquetes.json", "w") as f:
        json.dump(datos, f, indent=4)
def cargar_datos(sistema):
    if os.path.exists("paquetes.json"):
        with open("paquetes.json", "r") as f:
            datos = json.load(f)
            for item in datos:
                paquete = Paquete(
                    item["id"],
                    item["cliente"],
                    item["peso_kg"],
                    item["ciudad_destino"],
                    item["distancia_km"]
                )
                paquete.estado = item["estado"]
                sistema.paquetes.append(paquete)
            print("Datos cargados correctamente.")
            else:
                print("Opción inválida.")
    else:
        print("No se encontraron datos guardados.")
    else:
        print("No se encontraron datos guardados.")
                if orden["estado"] == "entregado":
                    orden["fecha_de_cierre"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")