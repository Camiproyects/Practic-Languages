"""
TERMINADO: NO.

1. La prueba está generada por IA.
2. Duración máxima: 1 hora y 30 minutos.
3. Puedes resolver todo en Python 3.
4. No uses librerías externas, solo estructuras básicas.
5. PROBLEMA GENERAL:
    Una empresa de cursos en línea llamada EduMaster necesita un sistema para
    gestionar estudiantes, suscripciones mensuales y pagos. El sistema debe
    permitir registrar estudiantes, activar o pausar suscripciones, calcular
    montos a pagar y generar reportes administrativos.

6. REQUERIMIENTOS FUNCIONALES (20 PUNTOS):

    1. (2 pts) Clase principal Estudiante:
       - Atributos: id, nombre, email, plan ("basico", "pro", "premium"),
         estado_suscripcion ("activa", "pausada").

    2. (2 pts) Clase SistemaSuscripciones:
       - Contiene una lista de estudiantes y métodos para gestionarlos.

    3. (2 pts) Método registrar_estudiante():
       - Solicita datos al usuario y crea un ID incremental.
       - Validar que el email no esté repetido.

    4. (2 pts) Método calcular_pago(estudiante):
       - Tarifas mensuales:
         - basico: 10 USD
         - pro: 20 USD
         - premium: 35 USD
       - Si la suscripción está pausada, el costo es 0.

    5. (1 pt) Método cambiar_plan(id_estudiante, nuevo_plan):
       - Validar que el plan sea uno de los tres disponibles.

    6. (1 pt) Método cambiar_estado(id_estudiante, nuevo_estado):
       - Solo acepta "activa" o "pausada".

    7. (2 pts) Método buscar_estudiante():
       - Permite buscar por nombre o email, ignorando mayúsculas.

    8. (3 pts) Método generar_reporte():
       - Debe mostrar:
         - total de estudiantes
         - número de suscripciones activas y pausadas
         - estudiantes por plan
         - ingresos mensuales proyectados (sumando calcular_pago())

    9. (3 pts) Persistencia con JSON:
       - Guardar los datos en "suscripciones.json".
       - Cargar automáticamente al iniciar.
       - Mantener la secuencia del ID.

    10. (1 pt) Código limpio y modular:
        - Comentarios claros, uso correcto de clases y separación de responsabilidades.

7. CASOS DE PRUEBA SUGERIDOS:
    - Registrar estudiante → Calcular pago.
    - Cambiar plan → Verificar actualización.
    - Cambiar estado → Validar que cambie el costo proyectado.
    - Generar reporte → Confirmar estadísticas.

8. ESTRUCTURA SUGERIDA:
    - Clase Estudiante
    - Clase SistemaSuscripciones
    - Funciones principales
    - Menú interactivo (texto)

"""

class Estudiante:
    def __init__(self, id, email, Plan, Est_Sub):
        self.id = id
        self.email = email
        self.Plan = Plan
        self.Est_Sub = Est_Sub
        