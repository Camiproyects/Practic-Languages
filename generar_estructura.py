import os, json

def generar_estructura(ruta_base):
    estructura = {}
    for root, dirs, files in os.walk(ruta_base):
        partes = os.path.relpath(root, ruta_base).split(os.sep)
        nodo = estructura
        for parte in partes:
            if parte == "." or parte.startswith(".git"):
                continue
            nodo = nodo.setdefault(parte, {})
        for archivo in files:
            if archivo.endswith(".json") or archivo.endswith(".pyc"):
                continue
            try:
                with open(os.path.join(root, archivo), 'r', encoding='utf-8') as f:
                    nodo[archivo] = f.read()
            except Exception as e:
                nodo[archivo] = f"[ERROR: {e}]"
    with open("estructura.json", "w", encoding="utf-8") as f:
        json.dump(estructura, f, indent=2, ensure_ascii=False)
    print("estructura.json regenerado correctamente.")

if __name__ == "__main__":
    generar_estructura(os.getcwd())
    print("Generando estructura del proyecto...")
    print("Estructura generada en 'estructura.json'.")