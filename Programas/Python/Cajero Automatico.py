"""
Ejemplo de cajero automatico 

Andres Camilo Laguna Bernal

16-01-2025
"""

import json
import os
import tkinter as tk
from tkinter import messagebox


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


def validar_telefono(telefono):
    """Valida que el teléfono tenga exactamente 10 dígitos y solo números."""
    if len(telefono) != 10 or not telefono.isdigit():
        return False
    return True


def validar_correo(correo):
    """Valida el formato del correo electrónico."""
    import re
    patron = r'^[a-zA-Z0-9_.+-]+@[a-zAZ0-9-]+\.[a-zA-Z0-9-.]+$'
    return bool(re.match(patron, correo))


def Acceso_Cliente(usuarios, telefono, pass_c, ventana_principal):
    """Verifica las credenciales de un cliente y muestra el menú de operaciones."""
    for usuario in usuarios:
        if usuario["Telefono"] == telefono and usuario["Pass_C"] == pass_c:
            messagebox.showinfo("Acceso Cliente", f"Bienvenido, {usuario['Nombre']}. Tu saldo actual es: {usuario['Saldo']}")
            mostrar_menu_operaciones(usuario, usuarios, ventana_principal)
            return
    messagebox.showerror("Error", "Credenciales incorrectas.")


def mostrar_menu_operaciones(usuario, usuarios, ventana_principal):
    """Muestra el menú de operaciones del cliente después de un acceso exitoso."""
    # Limpiar ventana
    for widget in ventana_principal.winfo_children():
        widget.destroy()

    tk.Label(ventana_principal, text=f"Bienvenido {usuario['Nombre']}", font=("Helvetica", 16)).pack(pady=10)

    def enviar_dinero():
        monto = entry_monto.get()
        telefono_destino = entry_telefono_destino.get()

        if not monto.isdigit() or float(monto) <= 0:
            messagebox.showerror("Error", "El monto debe ser un número positivo.")
            return

        monto = float(monto)
        if monto > usuario['Saldo']:
            messagebox.showerror("Error", "Saldo insuficiente.")
            return

        telefono_destino_completo = "+57" + telefono_destino
        for destinatario in usuarios:
            if destinatario["Telefono"] == telefono_destino_completo:
                usuario["Saldo"] -= monto
                destinatario["Saldo"] += monto
                Guardar_Usuarios(usuarios)
                messagebox.showinfo("Transacción Realizada", f"Has enviado {monto} a {destinatario['Nombre']}.")
                messagebox.showinfo("Saldo Actual", f"Tu nuevo saldo es: {usuario['Saldo']}")
                return
        messagebox.showerror("Error", "El destinatario no existe.")

    def retirar_dinero():
        monto = entry_monto.get()
        if not monto.isdigit() or float(monto) <= 0:
            messagebox.showerror("Error", "El monto debe ser un número positivo.")
            return

        monto = float(monto)
        if monto > usuario['Saldo']:
            messagebox.showerror("Error", "Saldo insuficiente.")
            return

        usuario["Saldo"] -= monto
        Guardar_Usuarios(usuarios)
        messagebox.showinfo("Transacción Realizada", f"Has retirado {monto}.")
        messagebox.showinfo("Saldo Actual", f"Tu nuevo saldo es: {usuario['Saldo']}.")

    def insertar_dinero():
        monto = entry_monto.get()
        if not monto.isdigit() or float(monto) <= 0:
            messagebox.showerror("Error", "El monto debe ser un número positivo.")
            return

        monto = float(monto)
        usuario["Saldo"] += monto
        Guardar_Usuarios(usuarios)
        messagebox.showinfo("Transacción Realizada", f"Has agregado {monto}.")
        messagebox.showinfo("Saldo Actual", f"Tu nuevo saldo es: {usuario['Saldo']}.")

    # Formulario para las operaciones
    tk.Label(ventana_principal, text="Número de Teléfono del Destinatario:").pack()
    entry_telefono_destino = tk.Entry(ventana_principal)
    entry_telefono_destino.pack(pady=5)

    tk.Label(ventana_principal, text="Monto:").pack()
    entry_monto = tk.Entry(ventana_principal)
    entry_monto.pack(pady=5)

    tk.Button(ventana_principal, text="Enviar Dinero", command=enviar_dinero).pack(pady=5)
    tk.Button(ventana_principal, text="Retirar Dinero", command=retirar_dinero).pack(pady=5)
    tk.Button(ventana_principal, text="Insertar Dinero", command=insertar_dinero).pack(pady=5)

    tk.Button(ventana_principal, text="Salir", command=ventana_principal.quit).pack(pady=10)


def Crear_Usuario(usuarios, nombre, correo_c, telefono, pass_c, ventana_principal):
    """Crea un usuario nuevo (cliente)."""
    if not validar_telefono(telefono):
        messagebox.showerror("Error", "El número de teléfono debe tener exactamente 10 dígitos y solo números.")
        return

    if not validar_correo(correo_c):
        messagebox.showerror("Error", "El correo electrónico no es válido.")
        return

    telefono_completo = "+57" + telefono
    saldo = 0

    for usuario in usuarios:
        if usuario["Correo_C"] == correo_c:
            messagebox.showerror("Error", "El correo ya está registrado.")
            return
        if usuario.get("Telefono") == telefono_completo:
            messagebox.showerror("Error", "El número de teléfono ya está registrado.")
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
    messagebox.showinfo("Usuario Creado", f"Usuario creado con éxito. Bienvenido, {nombre}.")
    mostrar_acceso_cliente(ventana_principal, usuarios)


def mostrar_acceso_cliente(ventana_principal, usuarios):
    """Ventana para acceso de cliente."""
    # Limpiar ventana
    for widget in ventana_principal.winfo_children():
        widget.destroy()

    tk.Label(ventana_principal, text="Acceso Cliente", font=("Helvetica", 16)).pack(pady=10)

    def verificar_acceso():
        telefono = entry_telefono.get()
        pass_c = entry_pass.get()
        Acceso_Cliente(usuarios, telefono, pass_c, ventana_principal)

    tk.Label(ventana_principal, text="Número de Teléfono:").pack()
    entry_telefono = tk.Entry(ventana_principal)
    entry_telefono.pack(pady=5)

    tk.Label(ventana_principal, text="Contraseña:").pack()
    entry_pass = tk.Entry(ventana_principal, show="*")
    entry_pass.pack(pady=5)

    tk.Button(ventana_principal, text="Acceder", command=verificar_acceso).pack(pady=10)


def mostrar_crear_usuario(ventana_principal, usuarios):
    """Ventana para crear un nuevo usuario."""
    # Limpiar ventana
    for widget in ventana_principal.winfo_children():
        widget.destroy()

    tk.Label(ventana_principal, text="Crear Usuario", font=("Helvetica", 16)).pack(pady=10)

    def crear_usuario():
        nombre = entry_nombre.get()
        correo_c = entry_correo.get()
        telefono = entry_telefono.get()
        pass_c = entry_pass.get()
        Crear_Usuario(usuarios, nombre, correo_c, telefono, pass_c, ventana_principal)

    tk.Label(ventana_principal, text="Nombre:").pack()
    entry_nombre = tk.Entry(ventana_principal)
    entry_nombre.pack(pady=5)

    tk.Label(ventana_principal, text="Correo:").pack()
    entry_correo = tk.Entry(ventana_principal)
    entry_correo.pack(pady=5)

    tk.Label(ventana_principal, text="Número de Teléfono:").pack()
    entry_telefono = tk.Entry(ventana_principal)
    entry_telefono.pack(pady=5)

    tk.Label(ventana_principal, text="Contraseña:").pack()
    entry_pass = tk.Entry(ventana_principal, show="*")
    entry_pass.pack(pady=5)

    tk.Button(ventana_principal, text="Crear Usuario", command=crear_usuario).pack(pady=10)


def ventana_principal():
    """Ventana principal con opciones de acceso."""
    usuarios = Cargar_Usuarios()

    ventana = tk.Tk()
    ventana.title("Gestor de Cajero Automático")
    ventana.geometry("400x400")  # Tamaño fijo

    mostrar_acceso_cliente(ventana, usuarios)

    ventana.mainloop()


if __name__ == "__main__":
    ventana_principal()
