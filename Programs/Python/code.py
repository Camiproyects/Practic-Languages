import turtle

# Configuración de pantalla
pantalla = turtle.Screen()
pantalla.bgcolor("white")
pantalla.title("Personaje inspirado en Pocoyó")

# Configuración del personaje
personaje = turtle.Turtle()
personaje.speed(5)
personaje.pensize(3)
personaje.penup()

# Función para dibujar un círculo relleno
def circulo_relleno(color, radio, x, y):
    personaje.goto(x, y)
    personaje.pendown()
    personaje.color(color)
    personaje.begin_fill()
    personaje.circle(radio)
    personaje.end_fill()
    personaje.penup()

# Dibujar cabeza
circulo_relleno("lightblue", 50, 0, 0)

# Dibujar gorro
circulo_relleno("blue", 55, 0, 20)

# Dibujar cara
circulo_relleno("white", 45, 0, -5)

# Dibujar ojos
circulo_relleno("black", 5, -15, 10)
circulo_relleno("black", 5, 15, 10)

# Dibujar boca
personaje.goto(-10, -10)
personaje.pendown()
personaje.setheading(-60)
personaje.circle(20, 120)
personaje.penup()

# Dibujar cuerpo
circulo_relleno("blue", 80, 0, -120)

# Dibujar brazos
circulo_relleno("blue", 20, -70, -50)
circulo_relleno("blue", 20, 70, -50)

# Dibujar pies
circulo_relleno("blue", 25, -30, -200)
circulo_relleno("blue", 25, 30, -200)

# Finalizar
personaje.hideturtle()
pantalla.mainloop()
import turtle

def draw_filled_circle(t, x, y, radius, fill_color, outline_color='black', outline_width=2):
    """
    Dibuja un círculo relleno con color de relleno (fill_color) y 
    un contorno de color outline_color y grosor outline_width.
    El círculo se dibuja con su centro en (x, y).
    """
    t.penup()
    # Ajuste para que el punto inicial sea la base del círculo
    t.goto(x, y - radius)
    t.setheading(0)
    t.pendown()
    t.color(outline_color, fill_color)
    t.pensize(outline_width)
    t.begin_fill()
    t.circle(radius)
    t.end_fill()
    t.penup()

def draw_head(t):
    """Dibuja la cabeza (un círculo de color azul claro)."""
    draw_filled_circle(t, 0, 100, 50, fill_color="lightblue")

def draw_hat(t):
    """Dibuja el gorro (un círculo azul ligeramente más grande que la cabeza)."""
    draw_filled_circle(t, 0, 125, 55, fill_color="blue")

def draw_face_details(t):
    """
    Dibuja los detalles de la cara:
    - Ojos negros (dos círculos)
    - Boca dibujada con un arco
    """
    # Ojo izquierdo
    draw_filled_circle(t, -15, 120, 5, fill_color="black")
    # Ojo derecho
    draw_filled_circle(t, 15, 120, 5, fill_color="black")

    # Boca (arco)
    t.goto(-10, 105)
    t.pendown()
    t.setheading(-60)
    t.color("black")
    t.pensize(2)
    # Dibuja un arco de 120 grados con radio 15
    t.circle(15, 120)
    t.penup()

def draw_body(t):
    """Dibuja el cuerpo (un gran círculo azul)."""
    draw_filled_circle(t, 0, 0, 60, fill_color="blue")

def draw_arms(t):
    """Dibuja los brazos (dos círculos azules a los costados)."""
    # Brazo izquierdo
    draw_filled_circle(t, -70, 0, 20, fill_color="blue")
    # Brazo derecho
    draw_filled_circle(t, 70, 0, 20, fill_color="blue")

def draw_legs(t):
    """Dibuja las piernas (dos círculos azules debajo del cuerpo)."""
    # Pierna izquierda
    draw_filled_circle(t, -30, -70, 20, fill_color="blue")
    # Pierna derecha
    draw_filled_circle(t, 30, -70, 20, fill_color="blue")

def draw_feet(t):
    """Dibuja los pies (dos círculos azules más pequeños)."""
    # Pie izquierdo
    draw_filled_circle(t, -30, -100, 15, fill_color="blue")
    # Pie derecho
    draw_filled_circle(t, 30, -100, 15, fill_color="blue")

def main():
    """Función principal que configura la pantalla y dibuja al personaje."""
    screen = turtle.Screen()
    screen.bgcolor("white")
    screen.title("Personaje inspirado en Pocoyó")

    t = turtle.Turtle()
    t.speed(7)  # Ajusta la velocidad (1 es lento, 10 es rápido)
    t.hideturtle()

    # Llamadas a las funciones de dibujo
    draw_head(t)
    draw_hat(t)
    draw_face_details(t)
    draw_body(t)
    draw_arms(t)
    draw_legs(t)
    draw_feet(t)

    screen.mainloop()

if __name__ == "__main__":
    main()
