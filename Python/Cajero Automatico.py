import json
import os

def ruta_archivo():
    """Devuelve la ruta completa del archivo JSON."""
    return os.path.join(os.path.dirname(__file__), "Cajero_Automatico.json")

def Cargar_Usuarios():
    """Carga los usuarios desde un archivo JSON."""
    archivo_json = ruta_archivo()
    if os.path.exists(archivo_json):
        with open(archivo_json, "r") as archivo:
            return json.load(archivo)
    return []

def Guardar_Usuarios(usuarios):
    """Guarda los usuarios en un archivo JSON."""
    archivo_json = ruta_archivo()
    with open(archivo_json, "w") as archivo:
        json.dump(usuarios, archivo, indent=4)

def Seleccion_De_Usuario():
    """Permite elegir el tipo de usuario."""
    print("\n--- Gestor de Usuarios ---")
    print("1. Acceder Como Cliente")
    print("2. Acceder Como Administrador")
    print("3. Crear Un Usuario Como Cliente")

def Acceso_Administrador():
    """Inicio de sesión para administrador."""
    print("\n--- Acceso Administrador ---")
    correo_a = input("Escribe tu correo: ")
    pass_a = input("Escribe tu contraseña: ")
    

def Acceso_Cliente(usuarios):
    """Inicio de sesión para cliente."""
    print("\n--- Acceso Cliente ---")
    correo_c = input("Escribe tu correo: ")
    pass_c = input("Escribe tu contraseña: ")

    for usuario in usuarios:
        if usuario["Correo_C"] == correo_c and usuario["Pass_C"] == pass_c:
            print(f"Bienvenido, {usuario['Nombre']}. Tu saldo actual es: {usuario['Saldo']}")
            while True:
                if usuario["Saldo"] == 0:
                    print("\nTu saldo es 0. Solo puedes insertar dinero.")
                    Insertar_Saldo(usuarios, correo_c)
                    return
                Operaciones_C()
                Operacion = input("Elige una operación: ")
                if Operacion == "1":
                    Enviar_Saldo(usuarios, correo_c)
                elif Operacion == "2":
                    Retirar_Saldo(usuarios, correo_c)
                elif Operacion == "3":
                    Insertar_Saldo(usuarios, correo_c)
                elif Operacion.lower() == "salir":
                    print("Saliendo del menú de operaciones.")
                    break
                else:
                    print("Opción no válida. Intenta nuevamente.")
            return
    print("Credenciales incorrectas. Intenta nuevamente.")

def Operaciones_C():
    """Muestra las opciones disponibles para el cliente."""
    print("\n¿Qué operación deseas realizar?")
    print("1. Enviar Dinero")
    print("2. Retirar Dinero")
    print("3. Insertar Dinero")
    print("Escribe 'salir' para volver al menú anterior.")

def Enviar_Saldo(usuarios, correo_c):
    """Permite enviar dinero a otro usuario usando su número de teléfono."""
    print("\n--- Enviar Dinero ---")
    telefono_destino = input("Escribe el número de teléfono del destinatario (sin el prefijo +57): ").strip()
    telefono_destino_completo = "+57" + telefono_destino 
    monto = float(input("Escribe el monto a enviar: "))

    for usuario in usuarios:
        if usuario["Correo_C"] == correo_c:
            if usuario["Saldo"] >= monto:
                for destinatario in usuarios:
                    if destinatario.get("Telefono") == telefono_destino_completo:
                        usuario["Saldo"] -= monto
                        destinatario["Saldo"] += monto
                        Guardar_Usuarios(usuarios)
                        print(f"Se enviaron {monto} a {destinatario['Nombre']} ({telefono_destino_completo}).")
                        print(f"Tu nuevo saldo es {usuario['Saldo']}.")
                        return
                print("El número de teléfono del destinatario no existe.")
                return
            else:
                print("Saldo insuficiente.")
                return
    print("Tu usuario no fue encontrado.")

def Retirar_Saldo(usuarios, correo_c):
    """Permite retirar dinero del saldo del usuario."""
    print("\n--- Retirar Dinero ---")
    monto = float(input("Escribe el monto a retirar: "))

    for usuario in usuarios:
        if usuario["Correo_C"] == correo_c:
            if usuario["Saldo"] >= monto:
                usuario["Saldo"] -= monto
                Guardar_Usuarios(usuarios)
                print(f"Has retirado {monto}. Tu nuevo saldo es {usuario['Saldo']}.")
                return
            else:
                print("Saldo insuficiente.")
                return
    print("Tu usuario no fue encontrado.")

def Insertar_Saldo(usuarios, correo_c):
    """Permite agregar dinero al saldo del usuario."""
    print("\n--- Insertar Dinero ---")
    monto = float(input("Escribe el monto a insertar: "))

    for usuario in usuarios:
        if usuario["Correo_C"] == correo_c:
            usuario["Saldo"] += monto
            Guardar_Usuarios(usuarios)
            print(f"Has agregado {monto}. Tu nuevo saldo es {usuario['Saldo']}.")
            return
    print("Tu usuario no fue encontrado.")

def Crear_Usuario(usuarios):
    """Crea un usuario nuevo (cliente)."""
    print("\n--- Crear Usuario ---")
    nombre = input("Escribe tu nombre: ")
    correo_c = input("Escribe tu correo: ")
    telefono = input("Escribe tu número de teléfono (sin el prefijo +57): ").strip()

    
    if not telefono.isdigit() or len(telefono) != 10:
        print("Error: El número de teléfono debe tener exactamente 10 dígitos y no contener letras ni símbolos.")
        return

    telefono_completo = "+57" + telefono

    pass_c = input("Escribe tu contraseña: ")
    saldo = 0

    for usuario in usuarios:
        
        if usuario["Correo_C"] == correo_c:
            print("Error: El correo ya está registrado.")
            return
        if usuario.get("Telefono") == telefono_completo:
            print("Error: El número de teléfono ya está registrado.")
            return

    nuevo_usuario = {
        "Nombre": nombre,
        "Correo_C": correo_c,
        "Telefono": telefono_completo,
        "Pass_C": pass_c,
        "Saldo": saldo,
    }
    usuarios.append(nuevo_usuario)
    Guardar_Usuarios(usuarios)
    print(f"Usuario creado con éxito. Bienvenido, {nombre}.")
    Acceso_Cliente(usuarios)


def main():
    usuarios = Cargar_Usuarios()
    while True:
        Seleccion_De_Usuario()
        opcion = input("Elige una opción: ")
        if opcion == "1":
            Acceso_Cliente(usuarios)
        elif opcion == "2":
            Acceso_Administrador()
        elif opcion == "3":
            Crear_Usuario(usuarios)
        else:
            print("Opción no válida. Intenta nuevamente.")

if __name__ == "__main__":
    main()
