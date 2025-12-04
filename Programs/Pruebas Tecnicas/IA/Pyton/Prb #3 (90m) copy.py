"""
================================================================================
TERMINADO: SI  //   SISTEMA DE GESTIÓN DE ENVÍOS - LOGITRACK 
================================================================================

DESCRIPCIÓN DEL PROBLEMA:
Una empresa de envíos llamada LogiTrack necesita un sistema para gestionar paquetes
que sus clientes envían a distintas ciudades. El sistema debe registrar paquetes,
calcular costos según peso y distancia, y generar reportes.

CARACTERÍSTICAS PRINCIPALES:
- Registrar nuevos paquetes con datos del cliente
- Calcular costos automáticamente (peso + distancia)
- Actualizar estado de entregas
- Buscar paquetes por cliente o ciudad
- Generar reportes con estadísticas
- Guardar y cargar datos en formato JSON

ESTRUCTURA DEL PROGRAMA:
1. Clase Paquete: Representa un paquete individual
2. Clase SistemaEnvios: Gestiona la lista de paquetes y operaciones
3. Funciones de persistencia: Guardar/cargar datos en JSON
4. Funciones de interfaz: Mostrar menú e interactuar con el usuario
5. Función main: Orquesta todo el programa

LIBRERÍAS UTILIZADAS:
- json: Para serializar/deserializar datos
- os: Para verificar existencia de archivos

ESTADO: TERMINADO Y COMPLETAMENTE FUNCIONAL
================================================================================
"""

# ============================================================================
# SECCIÓN 1: DEFINICIÓN DE LA CLASE PAQUETE
# ============================================================================
# Esta clase representa un PAQUETE individual que se envía a través del 
# sistema. Cada paquete tiene información del cliente, dimensiones, 
# ubicación de destino y un estado de entrega.

class Paquete:
    """
    Clase que modela un paquete en el sistema LogiTrack.
    
    Atributos:
    - id (int): Identificador único del paquete
    - cliente (str): Nombre de la persona que envía o recibe
    - peso_kg (float): Peso total del paquete en kilogramos
    - ciudad_destino (str): Ciudad de destino del envío
    - distancia_km (float): Distancia desde origen hasta destino en km
    - estado (str): Estado actual del envío ("en tránsito" o "entregado")
    """
    
    def __init__(self, id, cliente, peso_kg, ciudad_destino, distancia_km):
        """
        Constructor: inicializa un nuevo paquete con los datos proporcionados.
        
        Parámetros:
        - id: Identificador único (se asigna desde el sistema)
        - cliente: Nombre del cliente (string)
        - peso_kg: Peso en kilogramos (número decimal)
        - ciudad_destino: Nombre de la ciudad destino (string)
        - distancia_km: Distancia a recorrer en km (número decimal)
        
        El estado se inicializa automáticamente como "en tránsito".
        """
        self.id = id
        self.cliente = cliente
        self.peso_kg = peso_kg
        self.ciudad_destino = ciudad_destino
        self.distancia_km = distancia_km
        # Estado inicial por defecto
        self.estado = "en tránsito"

    def calcular_costo(self):
        """
        Calcula el costo de envío del paquete usando una fórmula matemática.
        
        FÓRMULA: costo = (peso_kg * 1000) + (distancia_km * 5)
        - Componente de peso: $1000 por kilogramo
        - Componente de distancia: $5 por kilómetro
        
        Retorna:
        - float: El costo total en dólares
        
        Ejemplo:
        - Paquete de 5 kg a 150 km: (5*1000) + (150*5) = 5750
        """
        return (self.peso_kg * 1000) + (self.distancia_km * 5)


# ============================================================================
# SECCIÓN 2: DEFINICIÓN DE LA CLASE SISTEMA DE ENVÍOS
# ============================================================================
# Esta clase es el "gestor central" del sistema. Mantiene una lista de todos
# los paquetes registrados y proporciona métodos para realizar operaciones
# como registrar, actualizar, buscar y generar reportes.

class SistemaEnvios:
    """
    Clase que gestiona todos los paquetes del sistema LogiTrack.
    
    Responsabilidades:
    - Almacenar una lista de paquetes
    - Permitir registrar nuevos paquetes
    - Actualizar estados de entrega
    - Listar paquetes según criterios
    - Buscar paquetes
    - Generar reportes estadísticos
    """
    
    def __init__(self):
        """
        Constructor: inicializa el sistema con una lista vacía de paquetes.
        Esta lista crecerá a medida que se registren nuevos envíos.
        """
        self.paquetes = []
    
    # ========================================================================
    # MÉTODO 1: Registrar un nuevo paquete
    # ========================================================================
    def registrar_paquete(self):
        """
        Solicita datos al usuario y crea un nuevo paquete en el sistema.
        
        PROCESO:
        1. Calcula un ID nuevo (basado en la cantidad de paquetes existentes)
        2. Solicita nombre del cliente
        3. Solicita peso en kilogramos (se convierte a float)
        4. Solicita ciudad destino
        5. Solicita distancia en kilómetros (se convierte a float)
        6. Crea un objeto Paquete con estos datos
        7. Lo agrega a la lista de paquetes
        8. Confirma el registro mostrando el ID asignado
        """
        # El nuevo ID es siempre la cantidad actual de paquetes + 1
        id = len(self.paquetes) + 1
        
        # Solicita información al usuario mediante input()
        cliente = input("Ingrese el nombre del cliente: ")
        peso_kg = float(input("Ingrese el peso en kg: "))
        ciudad_destino = input("Ingrese la ciudad destino: ")
        distancia_km = float(input("Ingrese la distancia en km: "))
        
        # Crea un objeto Paquete con los datos ingresados
        paquete = Paquete(id, cliente, peso_kg, ciudad_destino, distancia_km)
        
        # Agrega el paquete a la lista del sistema
        self.paquetes.append(paquete)
        
        # Confirmación al usuario
        print(f"Paquete registrado con ID: {id}")
    
    # ========================================================================
    # MÉTODO 2: Actualizar el estado de un paquete
    # ========================================================================
    def actualizar_estado(self, id_paquete, nuevo_estado):
        """
        Busca un paquete por ID y cambia su estado si es válido.
        
        PROCESO:
        1. Itera sobre todos los paquetes buscando el ID
        2. Si lo encuentra, valida que el nuevo estado sea válido
        3. Si es válido ("en tránsito" o "entregado"), lo actualiza
        4. Si no es válido, muestra error
        5. Si no encuentra el ID, muestra que no existe
        
        Parámetros:
        - id_paquete (int): ID del paquete a actualizar
        - nuevo_estado (str): Nuevo estado ("en tránsito" o "entregado")
        """
        # Itera sobre cada paquete en la lista
        for paquete in self.paquetes:
            # Si encuentra el paquete con el ID solicitado
            if paquete.id == id_paquete:
                # Verifica que el nuevo estado sea válido (está en la lista)
                if nuevo_estado in ["en tránsito", "entregado"]:
                    # Actualiza el estado
                    paquete.estado = nuevo_estado
                    print(f"Estado del paquete ID {id_paquete} actualizado a {nuevo_estado}")
                else:
                    # Error: estado no reconocido
                    print("Estado inválido. Use 'en tránsito' o 'entregado'.")
                return  # Termina la función, ya se encontró y procesó
        
        # Si llega aquí, el paquete no fue encontrado
        print("Paquete no encontrado.")
    
    # ========================================================================
    # MÉTODO 3: Listar paquetes por estado
    # ========================================================================
    def listar_paquetes_por_estado(self, estado):
        """
        Muestra todos los paquetes que coinciden con un estado específico.
        
        PROCESO:
        1. Recorre todos los paquetes del sistema
        2. Verifica si el estado coincide con el filtro
        3. Si coincide, imprime los datos del paquete
        
        Parámetro:
        - estado (str): Estado a filtrar ("en tránsito" o "entregado")
        """
        for paquete in self.paquetes:
            # Compara el estado del paquete con el estado filtrado
            if paquete.estado == estado:
                print(f"ID: {paquete.id}, Cliente: {paquete.cliente}, Ciudad: {paquete.ciudad_destino}, Estado: {paquete.estado}")
    
    # ========================================================================
    # MÉTODO 4: Buscar paquetes por criterio
    # ========================================================================
    def buscar_paquete(self, criterio):
        """
        Busca paquetes que contengan el criterio en cliente o ciudad.
        
        PROCESO:
        1. Convierte el criterio a minúsculas para búsqueda insensible
        2. Recorre todos los paquetes
        3. Verifica si el criterio está en el nombre del cliente (insensible)
        4. O si el criterio está en el nombre de la ciudad (insensible)
        5. Si coincide cualquiera, muestra el paquete
        
        Parámetro:
        - criterio (str): Texto a buscar (cliente o ciudad)
        
        NOTA: La búsqueda NO diferencia mayúsculas/minúsculas
        """
        # Convierte el criterio a minúsculas
        criterio = criterio.lower()
        
        # Itera sobre cada paquete
        for paquete in self.paquetes:
            # Comprueba si el criterio está en el cliente o en la ciudad (insensible a mayúsculas)
            if criterio in paquete.cliente.lower() or criterio in paquete.ciudad_destino.lower():
                print(f"ID: {paquete.id}, Cliente: {paquete.cliente}, Ciudad: {paquete.ciudad_destino}, Estado: {paquete.estado}")
    
    # ========================================================================
    # MÉTODO 5: Generar reporte estadístico
    # ========================================================================
    def generar_reporte(self):
        """
        Genera un reporte completo con estadísticas del sistema.
        
        PROCESO:
        1. Verifica que haya paquetes (si no, muestra mensaje)
        2. Cuenta el total de paquetes
        3. Cuenta cuántos están entregados
        4. Calcula cuántos están pendientes (total - entregados)
        5. Suma el peso total de todos los paquetes
        6. Suma el costo total de todos los envíos
        7. Calcula el costo promedio
        8. Imprime un reporte formateado
        """
        # Validación: ¿hay paquetes en el sistema?
        if not self.paquetes:
            print("No hay paquetes registrados.")
            return
        
        # ====== CÁLCULOS ======
        # Total de paquetes: contar elementos en la lista
        total_paquetes = len(self.paquetes)
        
        # Entregados: contar cuántos tienen estado "entregado"
        # sum() cuenta True como 1, False como 0
        entregados = sum(1 for p in self.paquetes if p.estado == "entregado")
        
        # Pendientes: total menos entregados
        pendientes = total_paquetes - entregados
        
        # Peso total: sumar el peso_kg de cada paquete
        peso_total = sum(p.peso_kg for p in self.paquetes)
        
        # Costo total: sumar el costo de cada paquete usando calcular_costo()
        costo_total = sum(p.calcular_costo() for p in self.paquetes)
        
        # Costo promedio: costo total dividido entre la cantidad
        # (si total_paquetes es 0, devuelve 0 para evitar división por cero)
        costo_promedio = costo_total / total_paquetes if total_paquetes > 0 else 0
        
        # ====== PRESENTACIÓN DEL REPORTE ======
        print("\n" + "="*50)
        print("REPORTE DE PAQUETES")
        print("="*50)
        print(f"Total de paquetes: {total_paquetes}")
        print(f"Entregados: {entregados}")
        print(f"Pendientes: {pendientes}")
        print(f"Peso total: {peso_total} kg")
        print(f"Costo promedio: ${costo_promedio:.2f}")
        print("="*50 + "\n")

# ============================================================================
# SECCIÓN 3: IMPORTACIÓN DE LIBRERÍAS
# ============================================================================
# Antes de implementar funciones, importamos las librerías que necesitamos

import json  # Para serializar/deserializar datos en formato JSON
import os    # Para verificar si archivos existen

# ============================================================================
# SECCIÓN 4: FUNCIONES DE PERSISTENCIA (Guardar/Cargar datos)
# ============================================================================
# Estas funciones permiten que el programa guarde los datos en disco
# y los recupere la próxima vez que se inicie.

def guardar_datos(sistema):
    """
    Guarda TODOS los paquetes del sistema en un archivo JSON.
    
    PROCESO:
    1. Crea una lista vacía llamada 'datos'
    2. Para cada paquete en el sistema:
       - Extrae sus atributos (id, cliente, peso_kg, etc.)
       - Los agrupa en un diccionario
       - Agrega el diccionario a la lista 'datos'
    3. Abre el archivo "paquetes.json" en modo escritura
    4. Convierte la lista de datos a formato JSON
    5. Lo escribe en el archivo con indentación (para legibilidad)
    6. Cierra el archivo
    7. Muestra confirmación al usuario
    
    FORMATO JSON RESULTANTE:
    [
        {
            "id": 1,
            "cliente": "Juan Perez",
            "peso_kg": 5.5,
            "ciudad_destino": "Bogota",
            "distancia_km": 150.0,
            "estado": "entregado"
        },
        ... más paquetes ...
    ]
    
    Parámetro:
    - sistema (SistemaEnvios): El sistema del cual extraer paquetes
    """
    # Lista que contendrá todos los datos a guardar
    datos = []
    
    # Itera sobre cada paquete del sistema
    for paquete in sistema.paquetes:
        # Crea un diccionario con los datos del paquete
        datos.append({
            "id": paquete.id,
            "cliente": paquete.cliente,
            "peso_kg": paquete.peso_kg,
            "ciudad_destino": paquete.ciudad_destino,
            "distancia_km": paquete.distancia_km,
            "estado": paquete.estado
        })
    
    # Abre el archivo en modo escritura ("w")
    # Si el archivo no existe, lo crea; si existe, lo sobrescribe
    with open("paquetes.json", "w") as f:
        # json.dump() convierte la lista a JSON y la escribe en el archivo
        # indent=4 agrega 4 espacios de indentación para legibilidad
        json.dump(datos, f, indent=4)
    
    # Confirmación al usuario
    print("Datos guardados correctamente en paquetes.json")


def cargar_datos(sistema):
    """
    Carga paquetes desde el archivo JSON al sistema.
    
    PROCESO:
    1. Verifica si el archivo "paquetes.json" existe
    2. Si NO existe, muestra un mensaje informativo
    3. Si SÍ existe:
       - Abre el archivo en modo lectura
       - Convierte el contenido JSON a una lista de diccionarios
       - Para cada diccionario en la lista:
         * Extrae los valores
         * Crea un objeto Paquete con esos valores
         * Restaura el estado del paquete
         * Lo agrega al sistema
       - Cierra el archivo
       - Muestra confirmación
    
    NOTA: Esta función se llama automáticamente cuando inicia el programa
    para restaurar los datos guardados previamente.
    
    Parámetro:
    - sistema (SistemaEnvios): El sistema en el cual cargar paquetes
    """
    # Verifica si el archivo existe usando os.path.exists()
    if os.path.exists("paquetes.json"):
        # Abre el archivo en modo lectura
        with open("paquetes.json", "r") as f:
            # json.load() convierte el contenido JSON a una estructura Python
            datos = json.load(f)
            
            # Itera sobre cada diccionario en la lista
            for item in datos:
                # Extrae los valores del diccionario y crea un Paquete
                paquete = Paquete(
                    item["id"],
                    item["cliente"],
                    item["peso_kg"],
                    item["ciudad_destino"],
                    item["distancia_km"]
                )
                
                # Restaura el estado (no se pasa en el constructor)
                paquete.estado = item["estado"]
                
                # Agrega el paquete restaurado al sistema
                sistema.paquetes.append(paquete)
            
            # Confirmación
            print("Datos cargados correctamente.")
    else:
        # No hay archivo guardado aún
        print("No se encontraron datos guardados.")

# ============================================================================
# SECCIÓN 5: FUNCIONES DE INTERFAZ DE USUARIO
# ============================================================================
# Estas funciones manejan la presentación y la interacción con el usuario

def mostrar_menu():
    """
    Muestra el menú principal de opciones del sistema.
    
    FUNCIÓN:
    - Imprime un menú formateado con líneas separadoras
    - Muestra 7 opciones diferentes
    - El usuario verá este menú repetidamente cada vez que realice una acción
    
    OPCIONES:
    1. Registrar nuevo paquete - Añadir un paquete al sistema
    2. Actualizar estado - Cambiar estado de un paquete
    3. Listar por estado - Ver paquetes filtrados
    4. Buscar paquete - Buscar por cliente o ciudad
    5. Generar reporte - Ver estadísticas
    6. Guardar datos - Persistir en JSON
    7. Salir - Terminar el programa
    
    NOTA: Esta función SOLO imprime, no valida entrada
    """
    print("\n" + "="*50)
    print("SISTEMA DE GESTIÓN DE ENVÍOS - LOGITRACK")
    print("="*50)
    print("1. Registrar nuevo paquete")
    print("2. Actualizar estado de paquete")
    print("3. Listar paquetes por estado")
    print("4. Buscar paquete")
    print("5. Generar reporte")
    print("6. Guardar datos")
    print("7. Salir")
    print("="*50)


# ============================================================================
# SECCIÓN 6: FUNCIÓN PRINCIPAL (ORQUESTADORA)
# ============================================================================
# Esta es la función que coordina todo el programa

def main():
    """
    Función principal que orquesta todo el sistema.
    
    FLUJO GENERAL:
    1. Crea un nuevo sistema (SistemaEnvios vacío)
    2. Carga datos guardados anteriormente desde JSON
    3. Entra en un bucle infinito que:
       - Muestra el menú
       - Solicita opción al usuario
       - Ejecuta la acción correspondiente
       - Repite hasta que el usuario seleccione "Salir"
    
    OPCIONES Y SU COMPORTAMIENTO:
    
    OPCIÓN 1: Registrar nuevo paquete
    - Llama a sistema.registrar_paquete()
    - El usuario ingresa los datos interactivamente
    - Se crea un nuevo Paquete y se agrega a la lista
    
    OPCIÓN 2: Actualizar estado
    - Solicita el ID del paquete
    - Solicita el nuevo estado
    - Valida que sea "en tránsito" o "entregado"
    - Actualiza si es válido
    - Incluye manejo de errores (si el ID no es número)
    
    OPCIÓN 3: Listar por estado
    - Solicita qué estado filtrar
    - Llama a sistema.listar_paquetes_por_estado()
    - Muestra todos los paquetes con ese estado
    - Enmarcado con líneas separadoras
    
    OPCIÓN 4: Buscar paquete
    - Solicita un criterio (cliente o ciudad)
    - Busca en todos los paquetes (insensible a mayúsculas)
    - Muestra los que coinciden
    - Enmarcado con líneas separadoras
    
    OPCIÓN 5: Generar reporte
    - Llama a sistema.generar_reporte()
    - Muestra estadísticas completas
    
    OPCIÓN 6: Guardar datos
    - Llama a guardar_datos(sistema)
    - Persiste todos los paquetes en paquetes.json
    
    OPCIÓN 7: Salir
    - Despide al usuario
    - Ejecuta "break" para salir del bucle
    - El programa termina
    
    VALIDACIÓN DE ENTRADA:
    - Si el usuario ingresa una opción inválida, muestra error
    - Si ingresa texto donde se espera número, lo captura con try/except
    """
    # Paso 1: Crear el sistema
    sistema = SistemaEnvios()
    
    # Paso 2: Cargar datos guardados (si existen)
    cargar_datos(sistema)
    
    # Paso 3: Bucle principal del programa
    while True:
        # Muestra el menú cada iteración
        mostrar_menu()
        
        # Solicita la opción al usuario y elimina espacios en blanco
        opcion = input("Seleccione una opción (1-7): ").strip()
        
        # ====== OPCIÓN 1: REGISTRAR PAQUETE ======
        if opcion == "1":
            sistema.registrar_paquete()
        
        # ====== OPCIÓN 2: ACTUALIZAR ESTADO ======
        elif opcion == "2":
            # try/except para manejar errores de entrada
            try:
                # Solicita el ID (debe ser número entero)
                id_paquete = int(input("Ingrese el ID del paquete: "))
                # Solicita el nuevo estado (se convierte a minúsculas)
                nuevo_estado = input("Ingrese el nuevo estado (en tránsito/entregado): ").lower()
                # Actualiza el estado
                sistema.actualizar_estado(id_paquete, nuevo_estado)
            except ValueError:
                # Si el usuario no ingresa un número válido
                print("ID inválido. Ingrese un número.")
        
        # ====== OPCIÓN 3: LISTAR POR ESTADO ======
        elif opcion == "3":
            # Solicita qué estado filtrar
            estado = input("Ingrese el estado a filtrar (en tránsito/entregado): ").lower()
            # Línea separadora
            print("\n" + "-"*50)
            
            # Bandera para detectar si se encontraron paquetes
            paquetes_encontrados = False
            
            # Itera sobre paquetes y muestra si coinciden
            for paquete in sistema.paquetes:
                if paquete.estado == estado:
                    print(f"ID: {paquete.id}, Cliente: {paquete.cliente}, Ciudad: {paquete.ciudad_destino}, Peso: {paquete.peso_kg} kg, Distancia: {paquete.distancia_km} km, Estado: {paquete.estado}, Costo: ${paquete.calcular_costo():.2f}")
                    paquetes_encontrados = True
            
            # Si no encontró ninguno, lo indica
            if not paquetes_encontrados:
                print("No hay paquetes con ese estado.")
            
            # Línea separadora
            print("-"*50)
        
        # ====== OPCIÓN 4: BUSCAR PAQUETE ======
        elif opcion == "4":
            # Solicita el criterio de búsqueda
            criterio = input("Ingrese el nombre del cliente o ciudad a buscar: ")
            # Línea separadora
            print("\n" + "-"*50)
            
            # Bandera para detectar si se encontraron paquetes
            paquetes_encontrados = False
            
            # Itera sobre paquetes buscando coincidencias (insensible a mayúsculas)
            for paquete in sistema.paquetes:
                if criterio.lower() in paquete.cliente.lower() or criterio.lower() in paquete.ciudad_destino.lower():
                    print(f"ID: {paquete.id}, Cliente: {paquete.cliente}, Ciudad: {paquete.ciudad_destino}, Peso: {paquete.peso_kg} kg, Distancia: {paquete.distancia_km} km, Estado: {paquete.estado}, Costo: ${paquete.calcular_costo():.2f}")
                    paquetes_encontrados = True
            
            # Si no encontró ninguno, lo indica
            if not paquetes_encontrados:
                print("No se encontraron paquetes con ese criterio.")
            
            # Línea separadora
            print("-"*50)
        
        # ====== OPCIÓN 5: GENERAR REPORTE ======
        elif opcion == "5":
            sistema.generar_reporte()
        
        # ====== OPCIÓN 6: GUARDAR DATOS ======
        elif opcion == "6":
            guardar_datos(sistema)
        
        # ====== OPCIÓN 7: SALIR ======
        elif opcion == "7":
            print("¡Hasta luego!")
            break  # Sale del bucle while, terminando el programa
        
        # ====== OPCIÓN INVÁLIDA ======
        else:
            print("Opción inválida. Intente de nuevo.")

# ============================================================================
# SECCIÓN 7: PUNTO DE ENTRADA DEL PROGRAMA
# ============================================================================
# Esta es la última línea de código que se ejecuta cuando se inicia el script

if __name__ == "__main__":
    """
    EXPLICACIÓN DE "__name__ == '__main__'":
    
    Cuando Python ejecuta un archivo:
    - Si el archivo se ejecuta directamente, __name__ tiene el valor "__main__"
    - Si el archivo se importa desde otro módulo, __name__ tiene el nombre del módulo
    
    Por lo tanto, este condicional asegura que main() SOLO se ejecute cuando:
    1. El usuario ejecuta el script directamente (python Prb #3 (90m).py)
    2. NO se ejecuta si otro script importa este archivo como módulo
    
    Esta es una MEJOR PRÁCTICA de Python para evitar efectos secundarios
    cuando se importan módulos.
    """
    # Inicia la ejecución del programa
    main()