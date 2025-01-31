from gestion_articulos import cargar_articulos, guardar_articulos
from gestion_usuarios import cargar_usuarios, guardar_usuarios
from alertas import generar_alertas_stock
from informes import generar_informe_mensual

def main():
    # Cargar datos
    articulos = cargar_articulos()
    usuarios = cargar_usuarios()

    # Generar alertas de stock
    generar_alertas_stock(articulos)

    # Generar informe mensual
    generar_informe_mensual(articulos)

if __name__ == "__main__":
    main()
