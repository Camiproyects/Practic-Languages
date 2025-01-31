from articles import add_article, update_article, delete_article, read_articles, generate_stock_alerts, generate_monthly_report
from users import add_user, authenticate_user, check_permissions
from utils import load_data, save_data, validate_expiry
import datetime

def main():
    articles = load_data('./data/articulos.json')
    users = load_data('./data/usuarios.json')
    
    # Ejemplo de añadir un artículo
    new_article = {
        "codbar": "xx99-xxx999-xxx99999",
        "codint": "abc123",
        "des": "Nuevo producto",
        "precom": 100.0,
        "preven": 150.0,
        "stk": 50,
        "cant": "Kg",
        "fecha": "31-01-2025"
    }
    
    # Validación de fecha de expiración
    if validate_expiry(new_article['fecha']):
        add_article(articles, new_article)
    else:
        print("Fecha de expiración no válida.")

    # Generar alertas de stock bajo
    generate_stock_alerts(articles)

    # Generar informe mensual
    generate_monthly_report(articles)

    # Ejemplo de autenticación de usuario
    user = authenticate_user(users, "1234567890", "unique123")
    if user and check_permissions(user, 'delete'):
        # Ejemplo de eliminar un artículo
        delete_article(articles, "abc123")
    else:
        print("No tiene permisos suficientes.")

if __name__ == "__main__":
    main()
