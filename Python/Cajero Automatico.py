import bcrypt
import mysql.connector
from mysql.connector import Error

def conectar_bd():
    """Establece conexión con la base de datos MySQL."""
    try:
        conexion = mysql.connector.connect(
            host="localhost",
            user="root",
            password="",
            database="Cajero_Automatico"
        )
        return conexion
    except Error as e:
        print(f"Error al conectar a la base de datos: {e}")
        return None

def inicializar_bd():
    """Inicializa la base de datos y crea la tabla si no existe."""
    conexion = conectar_bd()
    if conexion:
        cursor = conexion.cursor()
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS usuarios (
                id INT AUTO_INCREMENT PRIMARY KEY,
                nombre VARCHAR(255) NOT NULL,
                correo VARCHAR(255) UNIQUE NOT NULL,
                telefono VARCHAR(15) UNIQUE NOT NULL,
                contrasena VARCHAR(255) NOT NULL,
                saldo FLOAT NOT NULL DEFAULT 0
            )
        """)
        conexion.commit()
        cursor.close()
        conexion.close()

def cifrar_contrasena(telefono, contrasena):
    """
    Cifra una contraseña usando el número de teléfono como parte del proceso.
    """
    clave = telefono.encode("utf-8")  # Convertimos el teléfono en bytes
    contrasena = contrasena.encode("utf-8")  # Convertimos la contraseña en bytes
    return bcrypt.hashpw(contrasena + clave, bcrypt.gensalt())

def verificar_contrasena(telefono, contrasena, hash_contrasena):
    """
    Verifica si una contraseña es válida comparándola con su hash.
    """
    clave = telefono.encode("utf-8")
    contrasena = contrasena.encode("utf-8")
    return bcrypt.checkpw(contrasena + clave, hash_contrasena)

def guardar_usuario(nombre, correo, telefono, contrasena, saldo):
    """Guarda un nuevo usuario en la base de datos."""
    conexion = conectar_bd()
    if conexion:
        cursor = conexion.cursor()
        try:
            contrasena_cifrada = cifrar_contrasena(telefono, contrasena)
            cursor.execute("""
                INSERT INTO usuarios (nombre, correo, telefono, contrasena, saldo)
                VALUES (%s, %s, %s, %s, %s)
            """, (nombre, correo, telefono, contrasena_cifrada, saldo))
            conexion.commit()
            print("Usuario creado con éxito.")
        except Error as e:
            print(f"Error al guardar el usuario: {e}")
        finally:
            cursor.close()
            conexion.close()

def obtener_usuario_por_correo(correo):
    """Obtiene un usuario por su correo."""
    conexion = conectar_bd()
    if conexion:
        cursor = conexion.cursor(dictionary=True)
        cursor.execute("""
            SELECT * FROM usuarios
            WHERE correo = %s
        """, (correo,))
        usuario = cursor.fetchone()
        cursor.close()
        conexion.close()
        return usuario
    return None

def actualizar_saldo(correo, nuevo_saldo):
    """Actualiza el saldo de un usuario."""
    conexion = conectar_bd()
    if conexion:
        cursor = conexion.cursor()
        cursor.execute("""
            UPDATE usuarios
            SET saldo = %s
            WHERE correo = %s
        """, (nuevo_saldo, correo))
        conexion.commit()
        cursor.close()
        conexion.close()

def acceso_cliente():
    """Inicio de sesión para cliente."""
    print("\n--- Acceso Cliente ---")
    correo_c = input("Escribe tu correo: ")
    pass_c = input("Escribe tu contraseña: ")

    usuario = obtener_usuario_por_correo(correo_c)
    if usuario and verificar_contrasena(usuario["telefono"], pass_c, usuario["contrasena"].encode("utf-8")):
        print(f"Bienvenido, {usuario['nombre']}. Tu saldo actual es: {usuario['saldo']}")
        while True:
            operaciones_cliente()
            operacion = input("Elige una operación: ")
            if operacion == "1":
                enviar_saldo(correo_c)
            elif operacion == "2":
                retirar_saldo(correo_c)
            elif operacion == "3":
                insertar_saldo(correo_c)
            elif operacion.lower() == "salir":
                print("Saliendo del menú de operaciones.")
                break
            else:
                print("Opción no válida. Intenta nuevamente.")
    else:
        print("Credenciales incorrectas. Intenta nuevamente.")

def operaciones_cliente():
    """Muestra las opciones disponibles para el cliente."""
    print("\n¿Qué operación deseas realizar?")
    print("1. Enviar Dinero")
    print("2. Retirar Dinero")
    print("3. Insertar Dinero")
    print("Escribe 'salir' para volver al menú anterior.")

def insertar_saldo(correo):
    """Permite agregar dinero al saldo del usuario."""
    print("\n--- Insertar Dinero ---")
    try:
        monto = float(input("Escribe el monto a insertar: "))
    except ValueError:
        print("Error: Debes ingresar un monto válido.")
        return

    usuario = obtener_usuario_por_correo(correo)
    if usuario:
        nuevo_saldo = usuario["saldo"] + monto
        actualizar_saldo(correo, nuevo_saldo)
        print(f"Has agregado {monto}. Tu nuevo saldo es {nuevo_saldo}.")
    else:
        print("Usuario no encontrado.")

def crear_usuario():
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

    guardar_usuario(nombre, correo_c, telefono_completo, pass_c, saldo)

def seleccionar_usuario():
    """Permite elegir el tipo de usuario."""
    print("\n--- Gestor de Usuarios ---")
    print("1. Acceder Como Cliente")
    print("2. Acceder Como Administrador")
    print("3. Crear Un Usuario Como Cliente")

def main():
    inicializar_bd()
    while True:
        seleccionar_usuario()
        opcion = input("Elige una opción: ")
        if opcion == "1":
            acceso_cliente()
        elif opcion == "2":
            print("Función no implementada. Regresando al menú principal.")
        elif opcion == "3":
            crear_usuario()
        else:
            print("Opción no válida. Intenta nuevamente.")

if __name__ == "__main__":
    main()
