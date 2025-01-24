import pygame
import random
import math

# Configuraciones generales
ANCHO_PANTALLA = 300
ALTO_PANTALLA = 600
TAMAÑO_BLOQUE = 30
ANCHO_GRILLA = ANCHO_PANTALLA // TAMAÑO_BLOQUE
ALTO_GRILLA = ALTO_PANTALLA // TAMAÑO_BLOQUE
COLORES = [(0, 0, 0), (0, 255, 255), (0, 0, 255), (255, 165, 0),
           (0, 255, 0), (255, 0, 0), (128, 0, 128), (255, 255, 0)]

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
        return self.forma[self.rotacion % len(self.forma)]


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


def main():
    pygame.init()
    pantalla = pygame.display.set_mode((ANCHO_PANTALLA, ALTO_PANTALLA))
    pygame.display.set_caption("Tetris con IA")
    reloj = pygame.time.Clock()
    posiciones_bloqueadas = {}
    grilla = crear_grilla(posiciones_bloqueadas)
    pieza_actual = Pieza(5, 0, random.choice(FORMAS))
    puntuacion = 0
    tiempo_caida = 0
    velocidad_caida = 500

    corriendo = True
    while corriendo:
        grilla = crear_grilla(posiciones_bloqueadas)
        tiempo_caida += reloj.get_rawtime()
        reloj.tick()

        if tiempo_caida > velocidad_caida:
            tiempo_caida = 0
            pieza_actual.y += 1
            if not espacio_valido(pieza_actual, grilla) and pieza_actual.y > 0:
                pieza_actual.y -= 1
                agregar_a_bloqueadas(pieza_actual, posiciones_bloqueadas)
                pieza_actual = Pieza(5, 0, random.choice(FORMAS))
                puntuacion += 10
                if not espacio_valido(pieza_actual, grilla):
                    corriendo = False

        # Dibuja la pieza actual en la grilla temporal
        dibujar_pieza_actual(pieza_actual, grilla)
        dibujar_ventana(pantalla, grilla, puntuacion)
        pygame.display.update()


if __name__ == "__main__":
    main()
