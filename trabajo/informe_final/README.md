# Informe final — `ceosal1` (entrega 30 de noviembre de 2026)

Grupo: Jorge Figueroa, Patrick Peñaloza, Joaquín Rubio, Felipe Hormachea, Felipe Vidal, Fernanda Nuñez.

| Archivo | Qué es |
|---|---|
| [`informe_ceosal1.docx`](informe_ceosal1.docx) | Informe final en Word, editable (17 páginas). |
| [`informe_ceosal1.pdf`](informe_ceosal1.pdf) | El mismo informe en PDF, que es el formato que se entrega. |
| [`proyecto_ceosal1.ipynb`](proyecto_ceosal1.ipynb) | Cuaderno de Google Colab (R) con todo el código y una breve explicación de cada comando, ya ejecutado. |
| [`proyecto_ceosal1.R`](proyecto_ceosal1.R) | El mismo código como script para RStudio. |

## Antes de entregar

1. Subir `proyecto_ceosal1.ipynb` a Google Colab (*Archivo → Subir cuaderno*), cambiar el entorno a R si hace falta y *Ejecutar todas*.
2. Compartir el cuaderno (*Compartir → Cualquier persona con el enlace*) y pegar el enlace en el Anexo del informe (texto resaltado en amarillo).
3. Exportar el Word a PDF (*Archivo → Guardar como → PDF*) y entregar el PDF junto con el enlace o los archivos de código.

## Modelo y resultados principales

```
log(salary) = b0 + b1 log(sales) + b2 roe + b3 ros + b4 finance + b5 consprod + b6 utility + u
```

- Tamaño: elasticidad 0,26 (significativa).
- ROE: +1,1 % de salario por punto de ROE (significativo).
- Rentabilidad de la acción: sin efecto.
- Servicios públicos y transporte: cerca de 25 % menos salario (significativo).
- Hipótesis: se confirman H1, H2, H4 y H5; no se confirma H3.
