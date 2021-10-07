# SEFA: OSPA, Correos recordatorios
Desarrollo de herramienta para el envío masivo de correos recordatorios a las Entidades de Fiscalización Ambiental (EFA), por parte del Observatorio de Solución de Problemas Ambientales (OSPA). Este proyecto cumplió el objetivo de automatizar los correos recordatorios que periódicamente enviaba el OSPA a las EFA.

Al finalizar el proyecto se programó un script que se ejecuta de manera semanal y recoge la información consignada en el registro del OSPA, de tal forma, que selecciona y envía la información correspondiente a los requerimientos de información que las EFA deben responder.

# Archivos
- Auto_Recordatorio.R : Script en R para el envío de correos a la EFA.
- Carpeta de Imágenes:
    - Encabezado.png : Encabezado para los correos
    - Correo_EFA.png : Correo enviado a las EFA.
    - Correo_SEFA.png : Correo resumen de los recordatorios enviados a la EFA.

# Observaciones
- Se eliminó información sensible referida a correos electrónicos, ID de Google Sheets y accesos.
