import os
import json

# Archivos JSON principales
INDEX_FILE = r"C:\Users\camlo\Desktop\Practic-Languages\Programs\Python\json\index_music.json"
LYRICS_FILE = r"C:\Users\camlo\Desktop\Practic-Languages\Programs\Python\json\letras.json"
LRC_FOLDER = r"C:\Users\camlo\Music\SnapTube Audio"  # Carpeta donde guardas tus .lrc

# Asegurar que los JSON existen y no estén vacíos
def load_or_init(file_path, default_data):
    if not os.path.exists(file_path) or os.stat(file_path).st_size == 0:
        with open(file_path, "w", encoding="utf-8") as f:
            json.dump(default_data, f, ensure_ascii=False, indent=4)
        return default_data
    else:
        with open(file_path, "r", encoding="utf-8") as f:
            try:
                return json.load(f)
            except json.JSONDecodeError:
                return default_data

# Cargar o inicializar estructuras
index_music = load_or_init(INDEX_FILE, {})
lyrics = load_or_init(LYRICS_FILE, [])

# Función para generar ID nemotécnico de la canción
def generar_id(artista, titulo, contador):
    base = f"{artista[:3].upper()}_{titulo[:5].upper()}"
    return f"{base}_{contador:04d}"

# Procesar todos los .lrc de la carpeta
contador = len(lyrics) + 1
if not os.path.exists(LRC_FOLDER):
    print(f"❌ La carpeta '{LRC_FOLDER}' no existe. Por favor, créala y agrega archivos .lrc.")
    exit(1)
for archivo in os.listdir(LRC_FOLDER):
    if archivo.endswith(".lrc"):
        ruta = os.path.join(LRC_FOLDER, archivo)

        # Leer contenido
        with open(ruta, "r", encoding="utf-8") as f:
            lineas = f.readlines()

        # Extraer metadatos básicos del nombre del archivo
        # Ejemplo: "AlcolirykoZ - Medellificación (con Jerónimo).lrc"
        nombre_archivo = os.path.splitext(archivo)[0]
        partes = nombre_archivo.split(" - ")
        if len(partes) >= 2:
            artista, titulo = partes[0], partes[1]
        else:
            artista, titulo = "Desconocido", nombre_archivo

        genero = "Desconocido"  # De momento, hasta que se catalogue manualmente
        id_letra = generar_id(artista, titulo, contador)

        # Guardar en lyrics.json
        letras_obj = {
            "id_letra": id_letra,
            "lineas": [linea.strip() for linea in lineas if linea.strip()]
        }
        lyrics.append(letras_obj)

        # Guardar en index_music.json
        if genero not in index_music:
            index_music[genero] = {}
        if artista not in index_music[genero]:
            index_music[genero][artista] = []
        index_music[genero][artista].append({
            "titulo": titulo,
            "id_letra": id_letra
        })

        contador += 1

# Guardar actualizaciones en los JSON
with open(INDEX_FILE, "w", encoding="utf-8") as f:
    json.dump(index_music, f, ensure_ascii=False, indent=4)

with open(LYRICS_FILE, "w", encoding="utf-8") as f:
    json.dump(lyrics, f, ensure_ascii=False, indent=4)

print("✅ Volcado completado con éxito")
