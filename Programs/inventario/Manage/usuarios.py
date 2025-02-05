import json
import os

# Archivo donde COBOL escribe las operaciones (cola de instrucciones)
OPERACIONES_FILE = 'manage/usuarios.py'
# Archivo de datos principal en formato JSON (persistencia de usuarios)
USUARIOS_FILE = '../data/usuarios.json'

def leer_lineas_operaciones(archivo):
    """
    Lee cada línea del archivo de operaciones y retorna una lista de objetos JSON.
    Si el archivo no existe o hay error en la decodificación, se retorna una lista vacía.
    """
    operaciones = []
    if not os.path.exists(archivo):
        return operaciones
    try:
        with open(archivo, 'r', encoding='utf-8') as f:
            for linea in f:
                linea = linea.strip()
                if linea:
                    try:
                        operaciones.append(json.loads(linea))
                    except json.JSONDecodeError as e:
                        print("Error al decodificar la línea:", linea, "\n", e)
    except Exception as e:
        print("Error leyendo el archivo de operaciones:", e)
    return operaciones

def escribir_json(archivo, datos):
    """
    Escribe la estructura de datos (en formato JSON) en el archivo indicado.
    """
    try:
        with open(archivo, 'w', encoding='utf-8') as f:
            json.dump(datos, f, indent=4, ensure_ascii=False)
    except Exception as e:
        print("Error escribiendo en", archivo, ":", e)

def leer_usuarios():
    """
    Retorna la lista de usuarios almacenados en el archivo USUARIOS_FILE.
    Si el archivo no existe, se retorna una lista vacía.
    """
    if not os.path.exists(USUARIOS_FILE):
        return []
    try:
        with open(USUARIOS_FILE, 'r', encoding='utf-8') as f:
            data = json.load(f)
            return data.get("usuarios", [])
    except Exception as e:
        print("Error leyendo el archivo de usuarios:", e)
        return []

def crear_usuario(datos):
    """
    Agrega un nuevo usuario a la lista de usuarios.
    """
    usuarios = leer_usuarios()
    usuarios.append(datos)
    escribir_json(USUARIOS_FILE, {"usuarios": usuarios})
    print("Usuario creado.")

def actualizar_usuario(datos):
    """
    Actualiza un usuario existente identificándolo por tipdoc y numdoc.
    Si se encuentra, se actualizan todos los datos; de lo contrario se informa que no existe.
    """
    usuarios = leer_usuarios()
    actualizado = False
    for idx, usuario in enumerate(usuarios):
        if usuario.get("tipdoc") == datos.get("tipdoc") and usuario.get("numdoc") == datos.get("numdoc"):
            usuarios[idx] = datos
            actualizado = True
            break
    if actualizado:
        escribir_json(USUARIOS_FILE, {"usuarios": usuarios})
        print("Usuario actualizado.")
    else:
        print("Usuario con TIPDOC", datos.get("tipdoc"), "y NUMDOC", datos.get("numdoc"), "no encontrado.")

def eliminar_usuario(datos):
    """
    Elimina un usuario identificado por tipdoc y numdoc.
    Se compara la cantidad de registros antes y después para confirmar si se eliminó.
    """
    usuarios = leer_usuarios()
    cantidad_inicial = len(usuarios)
    usuarios = [u for u in usuarios if not (u.get("tipdoc") == datos.get("tipdoc") and u.get("numdoc") == datos.get("numdoc"))]
    if len(usuarios) < cantidad_inicial:
        escribir_json(USUARIOS_FILE, {"usuarios": usuarios})
        print("Usuario eliminado.")
    else:
        print("Usuario no encontrado para eliminar.")

def leer_operacion():
    """
    Muestra en consola el listado actual de usuarios.
    """
    usuarios = leer_usuarios()
    print("Usuarios actuales:")
    print(json.dumps({"usuarios": usuarios}, indent=4, ensure_ascii=False))

def procesar_operaciones():
    """
    Función principal que:
      - Lee el archivo de operaciones línea por línea.
      - Procesa cada operación (Crear, Leer, Actualizar o Eliminar) según el JSON recibido.
      - Al finalizar, limpia el archivo de operaciones.
    """
    operaciones = leer_lineas_operaciones(OPERACIONES_FILE)
    if not operaciones:
        print("No hay operaciones para procesar.")
        return
    for op in operaciones:
        operacion = op.get("operacion")
        datos = op.get("datos", {})
        if operacion == "C":
            crear_usuario(datos)
        elif operacion == "R":
            leer_operacion()
        elif operacion == "U":
            actualizar_usuario(datos)
        elif operacion == "D":
            eliminar_usuario(datos)
        else:
            print("Operación desconocida:", operacion)
    # Se limpia el archivo de operaciones una vez procesadas todas las instrucciones
    try:
        open(OPERACIONES_FILE, 'w', encoding='utf-8').close()
        print("Operaciones procesadas y archivo de operaciones limpiado.")
    except Exception as e:
        print("Error limpiando el archivo de operaciones:", e)

if __name__ == "__main__":
    procesar_operaciones()
