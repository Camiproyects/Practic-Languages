import subprocess
from datetime import datetime, timedelta
import time

hoy = datetime.now()
fecha_limite = datetime(1900, 1, 1)

fecha = hoy
while fecha >= fecha_limite:
    dia = fecha.strftime("%d/%m/%y")
    # Escribir el texto en el teléfono
    subprocess.run(["adb", "shell", "input", "text", dia])
    # Simular "Enter" para que pase a la siguiente línea
    subprocess.run(["adb", "shell", "input", "keyevent", "66"])
    time.sleep(30.5)  # medio segundo de pausa
    fecha -= timedelta(days=1)
