# Avance preliminar — `ceosal1` (1ra entrega, 28 de octubre de 2026)

Grupo: Jorge Figueroa, Patrick Peñaloza, Joaquín Rubio, Felipe Hormachea, Felipe Vidal, Fernanda Nuñez.

| Archivo | Qué es |
|---|---|
| [`avance_preliminar_ceosal1.ipynb`](avance_preliminar_ceosal1.ipynb) | Cuaderno de Google Colab (R) con el avance completo, ya ejecutado. Es el documento que piden las instrucciones. |
| [`avance_preliminar_ceosal1.pdf`](avance_preliminar_ceosal1.pdf) | El mismo cuaderno exportado a PDF, para revisarlo o adjuntarlo si se pide un archivo. |

## Cómo abrirlo en Colab

1. Entrar a <https://colab.research.google.com> → *Archivo → Subir cuaderno* y elegir el `.ipynb`
   (o abrirlo desde GitHub con el botón *Open in Colab* del propio cuaderno).
2. Si Colab lo abre con Python: *Entorno de ejecución → Cambiar tipo de entorno de ejecución → R*.
3. *Entorno de ejecución → Ejecutar todas*. La primera celda instala `wooldridge` y `corrplot` (tarda un par de minutos).
4. Para entregar: *Compartir → Cualquier persona con el enlace* y enviar el enlace.

## Contenido

1. Descripción de la base (origen, tipo de datos, diccionario de variables, revisión de nulos).
2. Medidas de tendencia central y dispersión, composición por sector y correlaciones.
3. Gráficos descriptivos (distribuciones, relaciones con el salario, salario por sector, valores atípicos).
4. Pregunta de investigación, justificación de variables, modelo poblacional e hipótesis H1–H5.

Modelo propuesto:

```
log(salary) = b0 + b1 log(sales) + b2 roe + b3 ros + b4 finance + b5 consprod + b6 utility + u
```
