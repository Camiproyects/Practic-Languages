# -*- coding: utf-8 -*-
"""
Name: Recreador de Imagen Progresivo
Description: Carga una imagen y la recrea progresivamente con líneas Bresenham
Author: Andres Camilo Laguna Bernal
Date: 2025-12-04
Terminado: Si
"""
import turtle
import random
from PIL import Image
import os

# Configuración
VENTANA_ANCHO = 800
VENTANA_ALTO = 600
IMAGEN_DEFECTO = "descarga.jpg"  # Imagen a cargar (debe estar en la carpeta)


def generar_imagen_defecto():
    """
    Si no existe imagen.png, crea una imagen de prueba simple
    (un círculo y rectángulo de colores).
    """
    if os.path.exists(IMAGEN_DEFECTO):
        return
    
    # Crear imagen de prueba con PIL si está disponible
    try:
        img = Image.new('RGB', (200, 200), color='white')
        pixels = img.load()
        
        # Dibujar un círculo rojo (simulado con píxeles)
        for x in range(200):
            for y in range(200):
                dx, dy = x - 100, y - 100
                if dx*dx + dy*dy < 50*50:  # círculo
                    pixels[x, y] = (255, 0, 0)
        
        # Dibujar un rectángulo azul
        for x in range(50, 150):
            for y in range(150, 200):
                pixels[x, y] = (0, 0, 255)
        
        img.save(IMAGEN_DEFECTO)
        print(f"Imagen de prueba creada: {IMAGEN_DEFECTO}")
    except Exception as e:
        print(f"No se pudo crear imagen de prueba: {e}")


def bresenham_linea(x0, y0, x1, y1):
    """
    Algoritmo de Bresenham para trazar una línea entre dos puntos.
    Devuelve una lista de coordenadas (x, y) que forman la línea.
    """
    puntos = []
    dx = abs(x1 - x0)
    dy = abs(y1 - y0)
    sx = 1 if x0 < x1 else -1
    sy = 1 if y0 < y1 else -1
    err = dx - dy
    
    x, y = x0, y0
    while True:
        puntos.append((x, y))
        if x == x1 and y == y1:
            break
        e2 = 2 * err
        if e2 > -dy:
            err -= dy
            x += sx
        if e2 < dx:
            err += dx
            y += sy
    
    return puntos


def detectar_bordes(imagen_rgb):
    """
    Detecta bordes usando un método simple (cambios de color).
    Devuelve una lista de puntos que representan los bordes detectados.
    """
    ancho, alto = imagen_rgb.size
    bordes = []
    
    # Redimensiona la imagen para trabajar más rápido
    imagen_small = imagen_rgb.resize((ancho // 4, alto // 4))
    pixels = imagen_small.load()
    w_small, h_small = imagen_small.size
    
    for y in range(h_small - 1):
        for x in range(w_small - 1):
            p1 = pixels[x, y]
            p2 = pixels[x + 1, y]
            p3 = pixels[x, y + 1]
            
            # Calcular diferencia de color
            diff1 = sum(abs(a - b) for a, b in zip(p1, p2))
            diff2 = sum(abs(a - b) for a, b in zip(p1, p3))
            
            # Si hay cambio significativo de color, es un borde
            if diff1 > 50 or diff2 > 50:
                bordes.append((x * 4, y * 4))
    
    return bordes


def cargar_imagen(ruta):
    """Carga una imagen desde archivo. Si no existe, intenta cargar defecto."""
    try:
        if os.path.exists(ruta):
            print(f"Cargando imagen: {ruta}")
            return Image.open(ruta).convert('RGB')
        else:
            print(f"Imagen no encontrada: {ruta}")
            generar_imagen_defecto()
            if os.path.exists(IMAGEN_DEFECTO):
                return Image.open(IMAGEN_DEFECTO).convert('RGB')
    except Exception as e:
        print(f"Error al cargar imagen: {e}")
    
    return None


def recrear_imagen_progresiva(imagen_rgb, turtle_obj, screen):
    """
    Recrea la imagen de forma progresiva usando líneas y bordes detectados.
    """
    if not imagen_rgb:
        print("No hay imagen para recrear")
        return
    
    # Detectar bordes
    print("Detectando bordes...")
    bordes = detectar_bordes(imagen_rgb)
    print(f"Bordes detectados: {len(bordes)}")
    
    # Barajar los bordes para un efecto más interesante
    random.shuffle(bordes)
    
    # Escalar coordenadas de bordes a la ventana
    ancho, alto = imagen_rgb.size
    escala_x = VENTANA_ANCHO / (ancho / 4)
    escala_y = VENTANA_ALTO / (alto / 4)
    offset_x = -VENTANA_ANCHO / 2
    offset_y = -VENTANA_ALTO / 2
    
    # Dibujar líneas progresivamente
    print("Recreando imagen...")
    turtle_obj.speed(0)
    turtle_obj.pensize(1)
    
    # Conectar bordes con líneas
    anterior = None
    colores_lista = ["red", "blue", "green", "purple", "orange", "cyan", "magenta", "yellow"]
    for i, borde in enumerate(bordes):
        x_pixel = int(borde[0] * escala_x + offset_x)
        y_pixel = int(borde[1] * escala_y + offset_y)
        
        # Limitar coordenadas
        x_pixel = max(-VENTANA_ANCHO // 2, min(VENTANA_ANCHO // 2, x_pixel))
        y_pixel = max(-VENTANA_ALTO // 2, min(VENTANA_ALTO // 2, y_pixel))
        
        # Color variado según posición (seleccionar de lista de colores)
        color_idx = int((i / len(bordes)) * (len(colores_lista) - 1))
        color = colores_lista[color_idx]
        turtle_obj.color(color)
        
        if anterior:
            # Dibujar línea Bresenham entre puntos anteriores
            linea = bresenham_linea(anterior[0], anterior[1], x_pixel, y_pixel)
            for (x, y) in linea:
                turtle_obj.goto(x, y)
                turtle_obj.pendown()
        else:
            turtle_obj.penup()
            turtle_obj.goto(x_pixel, y_pixel)
            turtle_obj.pendown()
        
        anterior = (x_pixel, y_pixel)
        
        # Actualizar pantalla cada 50 puntos
        if i % 50 == 0:
            screen.update()
            screen.title(f"Recreando imagen... {i}/{len(bordes)}")


def main():
    """
    Función principal: configura la pantalla, carga imagen y la recrea.
    """
    # Configurar pantalla
    screen = turtle.Screen()
    screen.setup(width=VENTANA_ANCHO, height=VENTANA_ALTO)
    screen.bgcolor("white")
    screen.title("Recreador de Imagen Progresivo")
    
    # Crear tortuga
    t = turtle.Turtle()
    t.hideturtle()
    t.penup()
    
    # Generar o cargar imagen
    generar_imagen_defecto()
    imagen = cargar_imagen(IMAGEN_DEFECTO)
    
    # Recrear imagen progresivamente
    if imagen:
        recrear_imagen_progresiva(imagen, t, screen)
        screen.title("Imagen recreada. Cierra la ventana para salir.")
    else:
        screen.title("Error: No se pudo cargar o generar imagen")
    
    screen.mainloop()


if __name__ == "__main__":
    main()
