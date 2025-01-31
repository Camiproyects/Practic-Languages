def generar_alertas_stock(articulos):
    min_stock = 10  # Stock mínimo definido
    for art in articulos:
        if art['stk'] < min_stock:
            print(f"ALERTA: Stock bajo para {art['des']} (Código: {art['codint']}). Stock actual: {art['stk']}")
