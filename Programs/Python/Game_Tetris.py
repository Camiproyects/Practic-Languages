# -*- coding: utf-8 -*-
"""
Name: Juego Tetris
Description: Programa que simula el juego de Tetris
Author: Andres Camilo Laguna Bernal
Date:
Terminado: SI (2024-06-15/"para mejorar")
"""
import pygame
import random
import math
import json
import os

# Configuraciones generales
ANCHO_PANTALLA = 300
ALTO_PANTALLA = 600
TAMAÑO_BLOQUE = 30
ANCHO_GRILLA = ANCHO_PANTALLA // TAMAÑO_BLOQUE
ALTO_GRILLA = ALTO_PANTALLA // TAMAÑO_BLOQUE
COLORES = [(0, 0, 0), (0, 255, 255), (0, 0, 255), (255, 165, 0),
           (0, 255, 0), (255, 0, 0), (128, 0, 128), (255, 255, 0)]

# Scoring estándar (simplificada) por número de líneas borradas
SCORE_LINES = {1: 100, 2: 300, 3: 500, 4: 800}

# Archivo para guardar el highscore
HIGHSCORE_FILE = "tetris_highscore.json"

FORMAS = [
    [[1, 1, 1, 1]],
    [[1, 1], [1, 1]],
    [[0, 1, 0], [1, 1, 1]],
    [[1, 0, 0], [1, 1, 1]],
    [[0, 0, 1], [1, 1, 1]],
    [[0, 1, 1], [1, 1, 0]],
    [[1, 1, 0], [0, 1, 1]]
]

class Pieza:
    def __init__(self, x, y, forma):
        self.x = x
        self.y = y
        self.forma = forma
        self.color = random.randint(1, len(COLORES) - 1)
        self.rotacion = 0

    def forma_rotada(self):
        """
        Devuelve la matriz 2D que representa la pieza en su rotación
        actual. Implementamos rotaciones aplicadas a la matriz base
        usando transposición y reverso (rotación de 90 grados).
        """
        matriz = self.forma
        # número de rotaciones de 90 grados a aplicar (0..3)
        r = self.rotacion % 4
        resultado = matriz
        for _ in range(r):
            # rotación 90 grados: transponer y revertir filas
            resultado = [list(fila) for fila in zip(*resultado[::-1])]
        return resultado


def crear_grilla(posiciones_bloqueadas={}):
    grilla = [[(0, 0, 0) for _ in range(ANCHO_GRILLA)] for _ in range(ALTO_GRILLA)]
    for (x, y), color in posiciones_bloqueadas.items():
        if y >= 0:
            grilla[y][x] = COLORES[color]
    return grilla


def espacio_valido(pieza, grilla):
    forma = pieza.forma_rotada()
    for y, fila in enumerate(forma):
        for x, celda in enumerate(fila):
            if celda:
                pos_x = pieza.x + x
                pos_y = pieza.y + y
                if pos_x < 0 or pos_x >= ANCHO_GRILLA or pos_y >= ALTO_GRILLA:
                    return False
                if pos_y >= 0 and grilla[pos_y][pos_x] != (0, 0, 0):
                    return False
    return True


def agregar_a_bloqueadas(pieza, posiciones_bloqueadas):
    forma = pieza.forma_rotada()
    for y, fila in enumerate(forma):
        for x, celda in enumerate(fila):
            if celda:
                posiciones_bloqueadas[(pieza.x + x, pieza.y + y)] = pieza.color


def borrar_lineas(grilla, posiciones_bloqueadas):
    """
    Elimina filas completas de la grilla y desplaza hacia abajo los
    bloques superiores. Devuelve el número de filas eliminadas.
    """
    filas_borradas = 0
    y = ALTO_GRILLA - 1
    # Recorremos de abajo hacia arriba
    while y >= 0:
        if all(grilla[y][x] != (0, 0, 0) for x in range(ANCHO_GRILLA)):
            filas_borradas += 1
            # Eliminar posiciones bloqueadas de esa fila
            for x in range(ANCHO_GRILLA):
                posiciones_bloqueadas.pop((x, y), None)
            # Mover todos los bloques por encima una fila hacia abajo
            nuevas_bloqueadas = {}
            for (bx, by), color in posiciones_bloqueadas.items():
                if by < y:
                    nuevas_bloqueadas[(bx, by + 1)] = color
                else:
                    nuevas_bloqueadas[(bx, by)] = color
            posiciones_bloqueadas.clear()
            posiciones_bloqueadas.update(nuevas_bloqueadas)
            # Reconstruir la grilla con las nuevas posiciones
            grilla = crear_grilla(posiciones_bloqueadas)
            # No disminuir y para re-evaluar la fila que ahora contiene
            # lo que antes estaba por encima
        else:
            y -= 1
    return filas_borradas


def dibujar_pieza_actual(pieza, grilla):
    forma = pieza.forma_rotada()
    for y, fila in enumerate(forma):
        for x, celda in enumerate(fila):
            if celda:
                pos_x = pieza.x + x
                pos_y = pieza.y + y
                if 0 <= pos_y < ALTO_GRILLA and 0 <= pos_x < ANCHO_GRILLA:
                    grilla[pos_y][pos_x] = COLORES[pieza.color]


def dibujar_ventana(pantalla, grilla, puntuacion):
    pantalla.fill((0, 0, 0))
    for y, fila in enumerate(grilla):
        for x, color in enumerate(fila):
            pygame.draw.rect(pantalla, color, (x * TAMAÑO_BLOQUE, y * TAMAÑO_BLOQUE, TAMAÑO_BLOQUE, TAMAÑO_BLOQUE), 0)
    fuente = pygame.font.Font(None, 30)
    texto_puntuacion = fuente.render(f"Puntuación: {puntuacion}", True, (255, 255, 255))
    pantalla.blit(texto_puntuacion, (10, 10))


def dibujar_preview(pantalla, pieza, x_offset, y_offset):
    """Dibuja una vista previa de una pieza en una pequeña área.
    `x_offset`, `y_offset` son coordenadas en píxeles donde empieza el preview."""
    matriz = pieza.forma_rotada()
    for ry, fila in enumerate(matriz):
        for rx, celda in enumerate(fila):
            if celda:
                pygame.draw.rect(pantalla, COLORES[pieza.color], (x_offset + rx * (TAMAÑO_BLOQUE // 2), y_offset + ry * (TAMAÑO_BLOQUE // 2), TAMAÑO_BLOQUE // 2, TAMAÑO_BLOQUE // 2))


def guardar_highscore(score):
    data = {"highscore": score}
    try:
        with open(HIGHSCORE_FILE, "w", encoding="utf-8") as f:
            json.dump(data, f)
    except Exception:
        pass


def cargar_highscore():
    if os.path.exists(HIGHSCORE_FILE):
        try:
            with open(HIGHSCORE_FILE, "r", encoding="utf-8") as f:
                data = json.load(f)
                return data.get("highscore", 0)
        except Exception:
            return 0
    return 0


def main():
    pygame.init()
    pantalla = pygame.display.set_mode((ANCHO_PANTALLA, ALTO_PANTALLA))
    pygame.display.set_caption("Tetris con IA")
    reloj = pygame.time.Clock()
    posiciones_bloqueadas = {}
    grilla = crear_grilla(posiciones_bloqueadas)
    pieza_actual = Pieza(5, 0, random.choice(FORMAS))
    siguiente_pieza = Pieza(5, 0, random.choice(FORMAS))
    hold_pieza = None
    hold_bloqueado = False  # evita multiple hold por una misma pieza
    puntuacion = 0
    tiempo_caida = 0
    velocidad_caida = 500
    nivel = 1
    lineas_totales = 0
    highscore = cargar_highscore()
    pausado = False
    mostrar_menu = True

    corriendo = True
    while corriendo:
        if mostrar_menu:
            # Pantalla de inicio simple
            pantalla.fill((0, 0, 0))
            fuente_g = pygame.font.Font(None, 40)
            texto = fuente_g.render("TETRIS - Presiona ENTER para comenzar", True, (255, 255, 255))
            pantalla.blit(texto, (10, ALTO_PANTALLA // 2 - 20))
            pygame.display.update()
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    return
                if event.type == pygame.KEYDOWN and event.key == pygame.K_RETURN:
                    mostrar_menu = False
            reloj.tick(10)
            continue

        grilla = crear_grilla(posiciones_bloqueadas)
        tiempo_caida += reloj.get_rawtime()
        reloj.tick()

        # Manejo de eventos (teclado y cerrar ventana)
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                corriendo = False
            if event.type == pygame.KEYDOWN:
                # Pausa
                if event.key == pygame.K_p:
                    pausado = not pausado
                if pausado:
                    continue
                # Movimiento horizontal
                if event.key == pygame.K_LEFT:
                    pieza_actual.x -= 1
                    if not espacio_valido(pieza_actual, grilla):
                        pieza_actual.x += 1
                elif event.key == pygame.K_RIGHT:
                    pieza_actual.x += 1
                    if not espacio_valido(pieza_actual, grilla):
                        pieza_actual.x -= 1
                # Movimiento hacia abajo (acelerar caída)
                elif event.key == pygame.K_DOWN:
                    pieza_actual.y += 1
                    if not espacio_valido(pieza_actual, grilla):
                        pieza_actual.y -= 1
                        # cuando la pieza baja y colisiona, bloqueamos la pieza
                        agregar_a_bloqueadas(pieza_actual, posiciones_bloqueadas)
                        lineas = borrar_lineas(grilla, posiciones_bloqueadas)
                        puntuacion += SCORE_LINES.get(lineas, 0) + 10
                        lineas_totales += lineas
                        pieza_actual = siguiente_pieza
                        siguiente_pieza = Pieza(5, 0, random.choice(FORMAS))
                        hold_bloqueado = False
                        if not espacio_valido(pieza_actual, grilla):
                            corriendo = False
                # Rotar la pieza
                elif event.key == pygame.K_UP:
                    pieza_actual.rotacion += 1
                    if not espacio_valido(pieza_actual, grilla):
                        pieza_actual.rotacion -= 1
                # Hard drop (bajar hasta tocar)
                elif event.key == pygame.K_SPACE:
                    while espacio_valido(pieza_actual, grilla):
                        pieza_actual.y += 1
                    pieza_actual.y -= 1
                    agregar_a_bloqueadas(pieza_actual, posiciones_bloqueadas)
                    lineas = borrar_lineas(grilla, posiciones_bloqueadas)
                    puntuacion += SCORE_LINES.get(lineas, 0) + 10
                    lineas_totales += lineas
                    pieza_actual = siguiente_pieza
                    siguiente_pieza = Pieza(5, 0, random.choice(FORMAS))
                    hold_bloqueado = False
                    if not espacio_valido(pieza_actual, grilla):
                        corriendo = False
                # Hold / swap de pieza
                elif event.key == pygame.K_c:
                    if not hold_bloqueado:
                        if hold_pieza is None:
                            hold_pieza = Pieza(5, 0, pieza_actual.forma)
                            pieza_actual = siguiente_pieza
                            siguiente_pieza = Pieza(5, 0, random.choice(FORMAS))
                        else:
                            # intercambiar forma y reiniciar posición
                            temp = Pieza(5, 0, pieza_actual.forma)
                            pieza_actual = Pieza(5, 0, hold_pieza.forma)
                            hold_pieza = temp
                        hold_bloqueado = True

        # Caída automática por tiempo (ajustamos velocidad por nivel)
        # Nivel aumenta cada 10 líneas completadas
        nivel = 1 + (lineas_totales // 10)
        # velocidad en ms: reduce con el nivel, con un mínimo
        velocidad_caida = max(50, 500 - (nivel - 1) * 40)
        if tiempo_caida > velocidad_caida and not pausado:
            tiempo_caida = 0
            pieza_actual.y += 1
            if not espacio_valido(pieza_actual, grilla) and pieza_actual.y > 0:
                pieza_actual.y -= 1
                agregar_a_bloqueadas(pieza_actual, posiciones_bloqueadas)
                # borrar filas completas y actualizar puntaje
                lineas = borrar_lineas(grilla, posiciones_bloqueadas)
                puntuacion += SCORE_LINES.get(lineas, 0) + 10
                lineas_totales += lineas
                pieza_actual = siguiente_pieza
                siguiente_pieza = Pieza(5, 0, random.choice(FORMAS))
                hold_bloqueado = False
                if not espacio_valido(pieza_actual, grilla):
                    corriendo = False

        # Dibuja la pieza actual en la grilla temporal
        dibujar_pieza_actual(pieza_actual, grilla)
        dibujar_ventana(pantalla, grilla, puntuacion)
        # Dibujar siguiente pieza (preview) y hold
        fuente = pygame.font.Font(None, 20)
        texto_next = fuente.render("Next:", True, (255, 255, 255))
        pantalla.blit(texto_next, (ANCHO_PANTALLA - 80, 10))
        dibujar_preview(pantalla, siguiente_pieza, ANCHO_PANTALLA - 80, 30)
        texto_hold = fuente.render("Hold:", True, (255, 255, 255))
        pantalla.blit(texto_hold, (ANCHO_PANTALLA - 80, 120))
        if hold_pieza:
            dibujar_preview(pantalla, hold_pieza, ANCHO_PANTALLA - 80, 140)
        # Mostrar nivel y highscore
        texto_nivel = fuente.render(f"Nivel: {nivel}", True, (255, 255, 255))
        pantalla.blit(texto_nivel, (ANCHO_PANTALLA - 80, 220))
        texto_high = fuente.render(f"High: {highscore}", True, (255, 255, 255))
        pantalla.blit(texto_high, (ANCHO_PANTALLA - 80, 240))
        pygame.display.update()
        # Actualizar highscore si es necesario
        if puntuacion > highscore:
            highscore = puntuacion
            guardar_highscore(highscore)


if __name__ == "__main__":
    main()
