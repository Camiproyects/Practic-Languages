from utils import save_data

def add_article(articles, article):
    articles['articulos'].append(article)
    save_data(articles, 'data/articulos.json')

def update_article(articles, codint, new_data):
    for article in articles['articulos']:
        if article['codint'] == codint:
            article.update(new_data)
            save_data(articles, 'data/articulos.json')
            return True
    return False

def delete_article(articles, codint):
    for article in articles['articulos']:
        if article['codint'] == codint:
            articles['articulos'].remove(article)
            save_data(articles, 'data/articulos.json')
            return True
    return False

def read_articles():
    articles = load_data('data/articulos.json')
    for article in articles['articulos']:
        print(article)

def generate_stock_alerts(articles):
    low_stock_items = [article for article in articles['articulos'] if article['stk'] < 10]
    for item in low_stock_items:
        print(f"Alerta: Stock bajo para el producto {item['des']} (Código Interno: {item['codint']})")

def generate_monthly_report(articles):
    report = {
        "fecha": datetime.datetime.now().strftime('%d-%m-%Y'),
        "total_articulos": len(articles['articulos']),
        "productos_por_categoria": {}
    }
    
    for article in articles['articulos']:
        categoria = article['categoria']
        if categoria not in report['productos_por_categoria']:
            report['productos_por_categoria'][categoria] = 0
        report['productos_por_categoria'][categoria] += 1
    
    report_path = f"reports/informe_mensual_{datetime.datetime.now().strftime('%Y_%m')}.json"
    save_data(report, report_path)
    print(f"Informe mensual generado: {report_path}")
