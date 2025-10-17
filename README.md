# R-tesis

Este repositorio contiene el codigo correspondiente a Tesina de grado en economía.
El objetivo del trabajo es analizar los **determinantes no económicos de la asistencia a la educación superior en Argentina** utilizando datos de la EPH y modelos de clasificación.

---

## 📁 Estructura del proyecto
├── master.R                # ejecuta el pipeline completo
├── 01_load_packages.R      # Instalación / carga de librerías
├── 02_load_data.R          # Carga de bases EPH (se esperan en /data)
├── 03_clean_data.R         # Limpieza y selección de muestra
├── 04_descriptive_data.R   # Estadísticas y tablas descriptivas
├── 05_variables.R          # Construcción de variables
├── 06_model_logit.R        # Estimaciones (modelos principales)
├── Tesis_Codigo.Rproj      # Proyecto de RStudio
└── (otros scripts varios de presentacion de resultados)

---

## 📦 Paquetes
- Las librerías necesarias se cargan automáticamente desde `01_load_packages.R`

---

## 📂 Datos

Los archivos **NO están incluidos en el repositorio** debido a su peso (>100MB).

Para ejecutar los scripts, se deben colocar en una carpeta local llamada `data/` los siguientes archivos:

| Nombre esperado | Formato |
|----------------|---------|
| `base_raw.csv` | CSV |
| `base_final.csv` | CSV |
| `usu_hogar_T324.xlsx` | Excel |
| `usu_hogar_T424.xlsx` | Excel |
| `usu_individuales_T324.xlsx` | Excel |
| `usu_individuales_T424.xlsx` | Excel |

📝 **Los datos se proporcionan por separado** (Google Drive) o pueden descargarse desde el sitio del INDEC (EPH 2024), renombrándolos según esta tabla.

---

## ▶️ Ejecución

1. Abrir el proyecto `Tesis_Codigo.Rproj` en RStudio  
2. Verificar que los archivos de datos estén presentes en `data/`  
3. Ejecutar `master.R` para reproducir todo el flujo

---
