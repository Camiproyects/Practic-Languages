import json

# Leer archivo JSON
def leer_json(archivo):
    try:
        with open(archivo, 'r', encoding='utf-8') as file:
            return json.load(file)
    except FileNotFoundError:
        print(f"Error: El archivo {archivo} no existe.")
        return {"usuarios": []}  # Retorna una estructura inicial vacía
    except json.JSONDecodeError:
        print(f"Error: El archivo {archivo} tiene un formato inválido.")
        return {"usuarios": []}

# Escribir datos en JSON
def escribir_json(archivo, datos):
    try:
        with open(archivo, 'w', encoding='utf-8') as file:
            json.dump(datos, file, indent=4, ensure_ascii=False)
    except Exception as e:
        print(f"Error al escribir en el archivo: {e}")

# Agregar nuevos datos al archivo JSON
def agregar_usuario(archivo, nuevo_usuario):
    datos = leer_json(archivo)
    if "usuarios" not in datos:
        datos["usuarios"] = []
    datos["usuarios"].append(nuevo_usuario)
    escribir_json(archivo, datos)

# Ejemplo de uso
if __name__ == "__main__":
    archivo_json = 'usuarios.json'

    # Leer los datos enviados por COBOL
    datos_cobol = leer_json(archivo_json)

    # Mostrar los datos recibidos
    print("Datos actuales en el archivo JSON:")
    print(json.dumps(datos_cobol, indent=4, ensure_ascii=False))

    # Agregar un nuevo usuario manualmente (como ejemplo)
    nuevo_usuario = {
        "id": 1234567890,
        "nombre": "María López",
        "correo": "maria.lopez@example.com"
    }
    agregar_usuario(archivo_json, nuevo_usuario)

    # Mostrar los datos actualizados
    print("Datos actualizados en el archivo JSON:")
    print(json.dumps(leer_json(archivo_json), indent=4, ensure_ascii=False))
