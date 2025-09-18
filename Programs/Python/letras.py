import tkinter as tk
from tkinter import ttk
import json
from difflib import get_close_matches
import pygame
import os

# Inicializar pygame mixer para audio
pygame.mixer.init()

# Ruta de tu carpeta de música
RUTA_MUSICA = r"C:\Users\camlo\Music\SnapTube Audio"

# Cargar JSON e índices
with open("C:/Users/camlo/Desktop/Practic-Languages/Programs/Python/json/index_music.json", "r", encoding="utf-8") as f:
    index_music = json.load(f)

with open("C:/Users/camlo/Desktop/Practic-Languages/Programs/Python/json/letras.json", "r", encoding="utf-8") as f:
    letras = json.load(f)

# Diccionarios para indexar géneros, artistas, canciones y letras.
indice_generos = {}
indice_artistas = {}
indice_canciones = {}
indice_letras = {}

for genero, artistas in index_music.items():
    for artista, canciones in artistas.items():
        for cancion in canciones:
            nombre = cancion["titulo"]
            id_letra = cancion["id_letra"]
            indice_generos.setdefault(genero.lower(), []).append(cancion)
            indice_artistas.setdefault(artista.lower(), []).append(cancion)
            indice_canciones[nombre.lower()] = cancion
            indice_letras[id_letra] = cancion

def buscar(criterio):
    criterio = criterio.lower()
    if criterio in indice_generos:
        return indice_generos[criterio]
    if criterio in indice_artistas:
        return indice_artistas[criterio]
    matches = get_close_matches(criterio, indice_canciones.keys(), n=3, cutoff=0.6)
    if matches:
        return [indice_canciones[m] for m in matches]
    resultados = []
    for letra in letras:
        id_letra = letra.get("id_letra")
        for bloque in letra.get("tiempos", []):
            for _, verso in bloque.items():
                if criterio in verso.lower():
                    cancion = indice_letras.get(id_letra, {}).copy()
                    cancion["preview"] = verso
                    resultados.append(cancion)
    return resultados

def reproducir(archivo):
    ruta_completa = os.path.join(RUTA_MUSICA, archivo)
    if os.path.exists(ruta_completa):
        pygame.mixer.music.load(ruta_completa)
        pygame.mixer.music.play()
    else:
        print(f"Archivo no encontrado: {ruta_completa}")

ventana = tk.Tk()
ventana.title("Reproductor de Música")
ventana.geometry("800x600")

frame_controles = tk.Frame(ventana)
frame_controles.pack(fill="x", padx=10, pady=5)

entrada = tk.Entry(frame_controles)
entrada.pack(side="left", fill="x", expand=True)

frame_resultados = tk.Frame(ventana)
frame_resultados.pack(fill="both", expand=True)

canvas = tk.Canvas(frame_resultados)
scrollbar = ttk.Scrollbar(frame_resultados, orient="vertical", command=canvas.yview)
scrollable_frame = tk.Frame(canvas)

scrollable_frame.bind(
    "<Configure>",
    lambda e: canvas.configure(scrollregion=canvas.bbox("all"))
)

canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")
canvas.configure(yscrollcommand=scrollbar.set)

canvas.pack(side="left", fill="both", expand=True)
scrollbar.pack(side="right", fill="y")

def mostrar():
    for widget in scrollable_frame.winfo_children():
        widget.destroy()
    criterio = entrada.get()
    resultados = buscar(criterio)
    if not resultados:
        tk.Label(scrollable_frame, text="No se encontraron resultados").pack()
        return
    for r in resultados:
        frame = tk.Frame(scrollable_frame, borderwidth=1, relief="solid", padx=5, pady=5)
        frame.pack(fill="x", padx=5, pady=5)
        info = (
            f"Género: {r.get('genero','-')}\n"
            f"Artista: {r.get('artista','-')}\n"
            f"Nombre: {r.get('titulo','-')}\n"
            f"Álbum: {r.get('album','-')}\n"
            f"Año: {r.get('año','-')}\n"
            f"ID Letra: {r.get('id_letra','-')}\n"
        )
        if "preview" in r:
            info += f"Coincidencia en letra: {r['preview']}\n"
        tk.Label(frame, text=info, justify="left", anchor="w").pack(side="left", fill="x", expand=True)
        archivo = r.get("archivo", None)
        if archivo:
            tk.Button(frame, text="▶ Reproducir", command=lambda a=archivo: reproducir(a)).pack(side="right")

boton_buscar = tk.Button(frame_controles, text="BUSCAR", command=mostrar)
boton_buscar.pack(side="left", padx=5)

ventana.mainloop()