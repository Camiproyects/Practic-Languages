# -*- coding: utf-8 -*-
"""
Name: Triángulos
Description: Programa que determina el tipo de triángulo y sus ángulos a partir de sus lados(mustra el dibujo del triángulo)
Author: Andres Camilo Laguna Bernal
Date:
Terminado: Si
"""

import math
import matplotlib.pyplot as plt
# pip install matplotlib

# Validaciones de datos
def validar_lado(lado):
    """Valida que el lado sea un número positivo."""
    if not isinstance(lado, (int, float)):
        raise ValueError("El valor del lado debe ser numérico.")
    if lado <= 0:
        raise ValueError("El valor del lado debe ser positivo.")
    return True

def validar_triangulo(a, b, c):
    """Valida que los lados cumplen con la desigualdad del triángulo."""
    if a + b <= c or a + c <= b or b + c <= a:
        raise ValueError("Los valores ingresados no forman un triángulo válido.")
    return True

# Cálculo de ángulos
def calcular_angulos(a, b, c):
    cos_A = (b**2 + c**2 - a**2) / (2 * b * c)
    A = math.degrees(math.acos(cos_A))

    cos_B = (a**2 + c**2 - b**2) / (2 * a * c)
    B = math.degrees(math.acos(cos_B))

    cos_C = (a**2 + b**2 - c**2) / (2 * a * b)
    C = math.degrees(math.acos(cos_C))

    return A, B, C

# Determinación del tipo de triángulo
def nombre_triangulo(a, b, c):
    if a == b == c:
        return "Equilátero"
    elif a == b or b == c or a == c:
        return "Isósceles"
    else:
        return "Escaleno"

def tipo_triangulo_por_angulo(angulos):
    if any(angulo > 90 for angulo in angulos):
        return "Obtusángulo"
    elif any(angulo == 90 for angulo in angulos):
        return "Rectángulo"
    else:
        return "Acutángulo"

# Dibujo del triángulo
def dibujar_triangulo(a, b, c, angulos):
    A = (0, 0)  
    B = (a, 0)
    C_x = (b**2 + a**2 - c**2) / (2 * a)
    C_y = math.sqrt(b**2 - C_x**2)
    C = (C_x, C_y)

    plt.figure()
    plt.plot([A[0], B[0]], [A[1], B[1]], 'b-')
    plt.plot([B[0], C[0]], [B[1], C[1]], 'b-')
    plt.plot([C[0], A[0]], [C[1], A[1]], 'b-')

    plt.text(A[0], A[1], 'A', fontsize=12, ha='right')
    plt.text(B[0], B[1], 'B', fontsize=12, ha='left')
    plt.text(C[0], C[1], 'C', fontsize=12, ha='center')

    plt.text(A[0]-0.1, A[1], f"{angulos[0]:.2f}°", fontsize=12, color='red')
    plt.text(B[0]+0.1, B[1], f"{angulos[1]:.2f}°", fontsize=12, color='red')
    plt.text(C[0], C[1]+0.1, f"{angulos[2]:.2f}°", fontsize=12, color='red')

    plt.xlim(-1, a + 1)
    plt.ylim(-1, b + 1)
    plt.axhline(0, color='black', linewidth=0.5, ls='--')
    plt.axvline(0, color='black', linewidth=0.5, ls='--')
    plt.grid(color='gray', linestyle='--', linewidth=0.5)
    plt.gca().set_aspect('equal', adjustable='box')
    plt.title(f"Triángulo {nombre_triangulo(a, b, c)}")
    plt.xlabel("Eje X")
    plt.ylabel("Eje Y")
    plt.show()

# Solicitud de entrada de datos con validación
try:
    a = float(input("Ingrese el lado a: "))
    b = float(input("Ingrese el lado b: "))
    c = float(input("Ingrese el lado c: "))

    # Validaciones
    validar_lado(a)
    validar_lado(b)
    validar_lado(c)
    validar_triangulo(a, b, c)

    # Cálculo y visualización del triángulo
    angulo_A, angulo_B, angulo_C = calcular_angulos(a, b, c)

    print(f"Tipo de triángulo: {nombre_triangulo(a, b, c)}")
    print(f"Ángulo A: {angulo_A:.2f}°")
    print(f"Ángulo B: {angulo_B:.2f}°")
    print(f"Ángulo C: {angulo_C:.2f}°")

    dibujar_triangulo(a, b, c, (angulo_A, angulo_B, angulo_C))

except ValueError as e:
    print(f"Error de entrada: {e}")