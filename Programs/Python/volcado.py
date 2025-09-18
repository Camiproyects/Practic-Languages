import os
import re
import json
from mutagen import File

# Carpeta de trabajo
CARPETA = r"C:\Users\camlo\Music\SnapTube Audio"
INDICE_JSON = os.path.join(CARPETA, "index_music.json")

def limpiar_nombre(nombre):
    # Elimina caracteres no válidos y espacios extra
    nombre = re.sub(r'[\\/:*?"<>|]', '', nombre)
    return nombre.strip()

def obtener_metadatos(ruta):
    audio = File(ruta, easy=True)
    if not audio:
        return {}
    return {
        "titulo": audio.get("title", [os.path.splitext(os.path.basename(ruta))[0]])[0],
        "artista": audio.get("artist", ["Desconocido"])[0],
        "album": audio.get("album", ["Desconocido"])[0],
        "genero": audio.get("genre", ["Desconocido"])[0],
        "año": audio.get("date", ["Desconocido"])[0],
        "duracion": int(audio.info.length) if audio.info else None,
        "bitrate": int(audio.info.bitrate / 1000) if audio.info and hasattr(audio.info, 'bitrate') else None
    }

def buscar_lrc(nombre_base):
    for archivo in os.listdir(CARPETA):
        if archivo.lower().endswith('.lrc') and nombre_base.lower() in archivo.lower():
            return archivo
    return None

indice = {}

for archivo in os.listdir(CARPETA):
    if archivo.lower().endswith(('.mp3', '.m4a', '.wav', '.flac', '.aac', '.ogg')):
        ruta_audio = os.path.join(CARPETA, archivo)
        meta = obtener_metadatos(ruta_audio)
        artista = limpiar_nombre(meta["artista"])
        titulo = limpiar_nombre(meta["titulo"])
        nombre_nuevo = f"{artista} - {titulo}{os.path.splitext(archivo)[1]}"
        ruta_nueva = os.path.join(CARPETA, nombre_nuevo)
        if archivo != nombre_nuevo:
            try:
                os.rename(ruta_audio, ruta_nueva)
            except FileExistsError:
                print(f"El archivo {ruta_nueva} ya existe. Se omitirá el renombrado de {archivo}.")
        # Buscar y asociar .lrc
        lrc_archivo = buscar_lrc(os.path.splitext(archivo)[0])
        letra = None
        if lrc_archivo:
            with open(os.path.join(CARPETA, lrc_archivo), encoding="utf-8") as f:
                letra = f.read()
            # Renombrar .lrc si es necesario
            nombre_lrc_nuevo = f"{artista} - {titulo}.lrc"
            ruta_lrc_nueva = os.path.join(CARPETA, nombre_lrc_nuevo)
            if lrc_archivo != nombre_lrc_nuevo:
                try:
                    os.rename(os.path.join(CARPETA, lrc_archivo), ruta_lrc_nueva)
                except FileExistsError:
                    print(f"El archivo {ruta_lrc_nueva} ya existe. Se omitirá el renombrado de {lrc_archivo}.")
        # Construir entrada en el índice
        genero = meta["genero"]
        if genero not in indice:
            indice[genero] = {}
        if artista not in indice[genero]:
            indice[genero][artista] = []
        cancion = {
            "titulo": titulo,
            "artista": artista,
            "album": meta["album"],
            "año": meta["año"],
            "duracion": meta["duracion"],
            "bitrate": meta["bitrate"],
            "archivo": nombre_nuevo,
            "letra_lrc": letra
        }
        indice[genero][artista].append(cancion)

# Guardar índice
with open(INDICE_JSON, "w", encoding="utf-8") as f:
    json.dump(indice, f, ensure_ascii=False, indent=4)

print("✅ Organización y generación de índice completada.")