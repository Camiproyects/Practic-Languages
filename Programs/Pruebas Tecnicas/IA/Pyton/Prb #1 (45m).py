"""
TRERMINADO: SI.

1.la prueba esta generada por IA.
2.tiene una duracion de 45 minutos.
3.puedes resolver todo en pyton 3.
4.no uses librerias externas solo estructuras basicas.
5.PRUEBA:
    5-1.crea un programa que pida un numero y diga si es primo o no.
    ej//ingrese un numero:7
        resultado: 7 es un numero primo
"""

num_pri = int(input("ingrese un numero: "))
contador1 = 1
divicible = 0
while contador1 <= num_pri+1:
        if num_pri%contador1 == 0:
            divicible =divicible + 1
            contador1 = contador1 + 1
        else:
            contador1 = contador1 + 1
if divicible == 2:
    print(num_pri," es un numero primo")
else:
    print(num_pri," no es un numero primo")

"""    5-2.haz un programa que reciba una palabra y cuente cuantas vocales tiene (a,e,i,o,u) el resiultado debe mostrar el total de cada vocal y el total en general.
    ej//ingrese una palabra: computadora
        resultado:
        a: 1
        e: 0
        i: 0
        o: 2
        u: 1
        total de vocales: 4
"""
        
palabra = str(input("ingrese la palabra: "))
a = 0
e = 0
i = 0
o = 0
u = 0
consonante = 0
for letra in palabra:
    if letra == "a":
         a = a + 1
    elif letra == "e":
          e = e + 1
    elif letra == "i":
          i = i + 1
    elif letra == "o":
          o = o + 1
    elif letra == "u":
          u = u + 1
    else:
          consonante = consonante +1
total = a+e+i+o+u
print("la palabra ", palabra, " tiene: ",
 "\n a: ", a,
 "\n e: ", e,
 "\n i: ", i,
 "\n o: ", o,
 "\n u: ", u,
 "\n total de vocales: ", total,
 "\n total consonates: ", consonante)

"""     5-3.pide al usuario cuantas notas0 quiere ingresar, guarda todas las notas en una lista calcula el promedio y muestra si el estudiante aprobo (>=3.0) o reprobo
     ej//ingrese cuantas notas desea ingresar: 3
     nota 1: 4.0
     nota 2: 3.5
     nota 3: 2.8
     promedio: 3.43
     resultado: aprobado
"""

numerodenotas = int(input("cuantas notas desea ingresar: "))
notas = []
contador3 = 1
contador4 = 1
while contador3 <= numerodenotas:
    notas.append(float(input("ingrese la nota: ")))
    contador3 = contador3 + 1

promedio = sum(notas)/numerodenotas

for nota in notas:
    print("\nnota",contador4,nota)
    contador4 = contador4 + 1
print("\n el proimdeio es",promedio,)

if promedio >= 3.0:
    print("\nel estudiante aprueba")
else:
    print("\nel estudiante no aprueba")