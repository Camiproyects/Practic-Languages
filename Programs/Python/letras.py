import tkinter as tk  # Importa el módulo tkinter para crear interfaces gráficas.
from tkinter import ttk  # Importa ttk para widgets mejorados de tkinter.
import json  # Permite trabajar con archivos y datos en formato JSON.
from difflib import get_close_matches  # Para encontrar coincidencias aproximadas entre cadenas.
import pygame  # Biblioteca para multimedia, aquí se usa para reproducir audio.
import os  # Proporciona funciones para interactuar con el sistema operativo.

# -----------------------------
# Inicializar pygame mixer para audio
# -----------------------------
pygame.mixer.init()  # Inicializa el módulo de audio de pygame.

# Ruta de tu carpeta de música
RUTA_MUSICA = "C:\Users\camlo\Music\SnapTube Audio"  # Define la ruta donde están los archivos de música.

# -----------------------------
# Cargar JSON e índices
# -----------------------------
with open("C:/Users/camlo/Desktop/Practic-Languages/Programs/Python/json/index_music.json", "r", encoding="utf-8") as f:
    index_music = json.load(f)  # Carga el índice de música desde un archivo JSON.

with open("C:/Users/camlo/Desktop/Practic-Languages/Programs/Python/json/letras.json", "r", encoding="utf-8") as f:
    letras = json.load(f)  # Carga las letras de las canciones desde otro archivo JSON.

# Diccionarios para indexar géneros, artistas, canciones y letras.
indice_generos = {}
indice_artistas = {}
indice_canciones = {}
indice_letras = {}

# Recorre el índice de música para poblar los diccionarios de búsqueda.
for genero, artistas in index_music.items():
    for artista, canciones in artistas.items():
        for cancion in canciones:
            nombre = cancion["nombre"]  # Obtiene el nombre de la canción.
            id_letra = cancion["id_letra"]  # Obtiene el ID de la letra.

            # Agrega la canción al índice de géneros.
            indice_generos.setdefault(genero.lower(), []).append(cancion)
            # Agrega la canción al índice de artistas.
            indice_artistas.setdefault(artista.lower(), []).append(cancion)
            # Asocia el nombre de la canción con su información.
            indice_canciones[nombre.lower()] = cancion
            # Asocia el ID de la letra con la canción.
            indice_letras[id_letra] = cancion

# -----------------------------
# Funciones auxiliares
# -----------------------------
def buscar(criterio):
    criterio = criterio.lower()  # Convierte el criterio de búsqueda a minúsculas.

    # Busca por género.
    if criterio in indice_generos:
        return indice_generos[criterio]

    # Busca por artista.
    if criterio in indice_artistas:
        return indice_artistas[criterio]

    # Busca coincidencias aproximadas en nombres de canciones.
    matches = get_close_matches(criterio, indice_canciones.keys(), n=3, cutoff=0.6)
    if matches:
        return [indice_canciones[m] for m in matches]

    # Busca coincidencias en las letras de las canciones.
    resultados = []
    for id_letra, contenido in letras.items():
        for bloque in contenido["tiempos"]:
            for _, verso in bloque.items():
                if criterio in verso.lower():
                    cancion = indice_letras[id_letra].copy()
                    cancion["preview"] = verso  # Guarda el verso donde se encontró la coincidencia.
                    resultados.append(cancion)
    return resultados  # Devuelve los resultados encontrados.

def reproducir(archivo):
    """Reproduce un archivo de audio usando pygame.mixer"""
    ruta_completa = os.path.join(RUTA_MUSICA, archivo)  # Construye la ruta completa del archivo de audio.
    if os.path.exists(ruta_completa):  # Verifica si el archivo existe.
        pygame.mixer.music.load(ruta_completa)  # Carga el archivo de audio.
        pygame.mixer.music.play()  # Reproduce el archivo de audio.
    else:
        print(f"Archivo no encontrado: {ruta_completa}")  # Muestra un mensaje si el archivo no existe.

# -----------------------------
# Interfaz Tkinter
# -----------------------------
ventana = tk.Tk()  # Crea la ventana principal de la aplicación.
ventana.title("Reproductor de Música")  # Establece el título de la ventana.
ventana.geometry("800x600")  # Define el tamaño de la ventana.

entrada = tk.Entry(ventana)  # Crea un campo de entrada de texto para el criterio de búsqueda.
entrada.pack(fill="x", padx=10, pady=5)  # Lo coloca en la ventana con relleno.

frame_resultados = tk.Frame(ventana)  # Crea un marco para mostrar los resultados.
frame_resultados.pack(fill="both", expand=True)  # Lo coloca y expande en la ventana.

canvas = tk.Canvas(frame_resultados)  # Crea un canvas para permitir el desplazamiento de los resultados.
scrollbar = ttk.Scrollbar(frame_resultados, orient="vertical", command=canvas.yview)  # Barra de desplazamiento vertical.
scrollable_frame = tk.Frame(canvas)  # Frame interno que será desplazable.

# Actualiza la región de desplazamiento cuando cambia el tamaño del frame interno.
scrollable_frame.bind(
    "<Configure>",
    lambda e: canvas.configure(scrollregion=canvas.bbox("all"))
)

canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")  # Inserta el frame desplazable en el canvas.
canvas.configure(yscrollcommand=scrollbar.set)  # Asocia la barra de desplazamiento al canvas.

canvas.pack(side="left", fill="both", expand=True)  # Coloca el canvas a la izquierda y lo expande.
scrollbar.pack(side="right", fill="y")  # Coloca la barra de desplazamiento a la derecha.

def mostrar():
    # Limpia los resultados anteriores.
    for widget in scrollable_frame.winfo_children():
        widget.destroy()

    criterio = entrada.get()  # Obtiene el texto ingresado por el usuario.
    resultados = buscar(criterio)  # Busca resultados según el criterio.

    if not resultados:
        tk.Label(scrollable_frame, text="No se encontraron resultados").pack()  # Muestra mensaje si no hay resultados.
        return

    # Muestra cada resultado en la interfaz.
    for r in resultados:
        frame = tk.Frame(scrollable_frame, borderwidth=1, relief="solid", padx=5, pady=5)
        frame.pack(fill="x", padx=5, pady=5)

        info = (
            f"Género: {r.get('genero','-')}\n"
            f"Artista: {r.get('artista','-')}\n"
            f"Nombre: {r.get('nombre','-')}\n"
            f"Álbum: {r.get('album','-')}\n"
            f"Año: {r.get('año','-')}\n"
            f"ID Letra: {r.get('id_letra','-')}\n"
        )
        if "preview" in r:
            info += f"Coincidencia en letra: {r['preview']}\n"  # Muestra el verso donde se encontró la coincidencia.

        tk.Label(frame, text=info, justify="left", anchor="w").pack(side="left", fill="x", expand=True)

        # Botón para reproducir la canción si existe el archivo.
        archivo = r.get("archivo", None)  # Obtiene el nombre del archivo de audio.
        if archivo:
            tk.Button(frame, text="▶ Reproducir", command=lambda a=archivo: reproducir(a)).pack(side="right")

boton_buscar = tk.Button(ventana, text="BUSCAR", command=mostrar)  # Botón para iniciar la búsqueda.
boton_buscar.pack(pady=5)  # Lo coloca en la ventana con relleno.

ventana.mainloop()  # Inicia el bucle principal de la interfaz gráfica.