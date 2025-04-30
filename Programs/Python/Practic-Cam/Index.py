import cv2
import pytesseract

# Dirección IP de la cámara (ajústala según lo que muestra IP Webcam)
url = "http://192.168.0.13:8080/video"

# Iniciar captura de video
cap = cv2.VideoCapture(url)

while True:
    ret, frame = cap.read()
    if not ret:
        break

    # Convertir la imagen a escala de grises (mejora el OCR)
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

    # Aplicar OCR para extraer texto
    texto_extraido = pytesseract.image_to_string(gray, lang='eng')  # Usa 'spa' para español

    # Mostrar la imagen y el texto extraído
    cv2.imshow("Camara del Android", frame)
    print("Texto Detectado:\n", texto_extraido)

    # Salir con 'q'
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break

cap.release()
cv2.destroyAllWindows()
