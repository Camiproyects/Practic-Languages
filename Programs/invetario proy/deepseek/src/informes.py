import json
from datetime import datetime

def generar_informe_mensual(articulos):
    total_stock = sum(art['stk'] for art in articulos)
    total_value = sum(art['stk'] * art['preven'] for art in articulos)
    report = {
        "fecha": datetime.now().strftime("%Y-%m"),
        "total_stock": total_stock,
        "total_value": total_value,
        "articulos": articulos
    }
    with open('../data/informes.json', 'w') as f:
        json.dump(report, f, indent=4)
    print("Informe mensual generado con éxito.")
