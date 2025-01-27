# -*- coding: utf-8 -*-
"""
Name: Adivina el numero
Description: Juego en el que el usuario debe adivinar un número aleatorio
Author: Andres Camilo Laguna Bernal
Date:
Terminado: Si
"""
import random

def again():
    option = str(input("Deseas Jugar de nuevo? yes/no:"))
    if option == "yes":
        new_game()
    else:
        print("end")

def in_game(vidas, num_pc):
    intentos = 0
    while vidas != 0:
        intentos = intentos + 1
        num_ju = int(input("Inserta el numero: "))
        if num_ju != num_pc:
            if num_ju < num_pc:
                print("El numero es mayor")
                vidas = vidas -1
            else:
                print("El numero es menor")
                vidas =vidas -1
        else:
            print("ADIVINASTE TE TOMÓ " , intentos , " intentos")
            again()
    else:
        print("Te has quedado sin vidas Era el" , num_pc)
        again()

def new_game():
    
    print (" -- selecciona la dificultad                  -- ")
    print (" -- 1. dificultad facil      3/vidas 1/digitos-- ")
    print (" -- 2. dificultad media      4/vidas 2/digitos-- ")
    print (" -- 3. dificultad dificil    6/vidas 3/digitos-- ")
    print (" -- 4. dificultad impocible  10/vidas 4/digitos-- ")
    
    option = int(input("selecciona una opcion: "))
    
    if option == 1:
        vidas = 3
        num_pc = int(random.randint(0,9))
        #print (vidas)
        #print (num_pc)
        in_game(vidas, num_pc)
    elif option == 2:
        vidas = 4
        num_pc = int(random.randint(10,99))
        #print (vidas)
        #print (num_pc)
        in_game(vidas, num_pc)
    elif option == 3:
        vidas = 6
        num_pc = int(random.randint(100,999))
        #print (vidas)
        #print (num_pc)
        in_game(vidas, num_pc)
    else:
        vidas = 10
        num_pc = int(random.randint(1000,9999))
        #print (vidas)
        #print (num_pc)
        in_game(vidas, num_pc)

if __name__ == "__main__":
    new_game()