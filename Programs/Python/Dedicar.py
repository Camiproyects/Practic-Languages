# -*- coding: utf-8 -*-
"""
Name: Dedicatoria
Description: 
Author: Andres Camilo Laguna Bernal
Date:
Terminado: No
"""
import os
from twilio.rest import Client

# Configura tus credenciales de Twilio desde variables de entorno
account_sid = os.getenv('TWILIO_ACCOUNT_SID')
auth_token = os.getenv('TWILIO_AUTH_TOKEN')
client = Client(account_sid, auth_token)

# Lista de números a los que deseas enviar mensajes
numeros_destino = ['whatsapp:+573222627754']

for numero in numeros_destino:
    message = client.messages.create(
        body='Hola, este es un mensaje de prueba desde Python',
        from_='whatsapp:+14155238886',  # Número de WhatsApp del Sandbox de Twilio
        to=numero
    )
    print(f"Mensaje enviado a {numero}: {message.sid}")
