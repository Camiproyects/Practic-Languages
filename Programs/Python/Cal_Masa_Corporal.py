# -*- coding: utf-8 -*-
"""
Name: Calculadora de Masa Corporal
Description: Programa Diceñado Para Calcular El Indice De Masa Corporal (IMC)
Author: Andres Camilo Laguna Bernal
Date:
Terminado: Si
"""
try:
    print("-- Esto es un simple programa para medir el índice de masa corporal --")
    # Toma de datos
    ESTATURA = float(input("Por favor, ingrese su estatura (Metros): "))
    PESO = float(input("Por favor, ingrese su peso (Kg): "))
    
    # Validación de datos
    if ESTATURA <= 0 or PESO <= 0:
        print("Datos incorrectos, la estatura y el peso deben ser mayores a 0.")
        
    else:
        # Cálculo del índice de masa corporal (IMC)
        IMC = PESO / (ESTATURA ** 2)
        print(f"Su índice de masa corporal (IMC) es: {IMC:.2f}")
    
        if IMC < 16:
            print("Se encuentra en delgadez severa")
        elif 16 <= IMC < 17:
            print("Se encuentra en delgadez moderada")
        elif 17 <= IMC < 18.5:
            print("Se encuentra en delgadez leve")
        elif 18.5 <= IMC < 25:
            print("Se encuentra en peso normal")
        elif 25 <= IMC < 30:
            print("Se encuentra en sobrepeso")
        elif 30 <= IMC < 35:
            print("Se encuentra en obesidad leve")
        elif 35 <= IMC < 40:
            print("Se encuentra en obesidad moderada")
        else:
            print("Se encuentra en obesidad mórbida")

except ValueError:
    print("Recuerda que los valores deben ser numéricos.")
