import math

while True:
    try:
        num1 = int(input("ingresa el numerador : "))
        num2 = int(input("ingresa el denominador : "))
        
        print(f"\n Simplificado {num1}/{num2} paso a paso :")
        paso = 1
        
        while True:
            mcd = math.gcd(num1,num2)
            
            if mcd == 1:
                print(f"Paso {paso} : {num1}/{num2} ya no se pueden simplificar mas")
                break
            
            num1 //= mcd
            num2 //= mcd
            print(f"Paso {paso} : dividido entre {mcd} -> /{num1}\ /{num2}\ ")
            
            salir = input("")
            if salir.lower == 's':
                break
            paso += 1
            
                        
    except ValueError:
        print('againg :p')