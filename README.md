
<!-- README.md is generated from README.Rmd. Please edit that file -->

\|\|\|\|\|\| **Work in progress** \|\|\|\|\|\| **En construcción**
\|\|\|\|\|\|

# The ggcleveland R package

\|\|\|\|\|\| **Work in progress** \|\|\|\|\|\| **En construcción**
\|\|\|\|\|\|

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/karel)](https://CRAN.R-project.org/package=ggcleveland)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Downloads](https://cranlogs.r-pkg.org/badges/ggcleveland?color=blue)](https://cran.rstudio.com/package=ggcleveland)
<!-- badges: end -->

Web: <https://mpru.github.io/ggcleveland/>

Este paquete implementa algunas funciones para crear las versiones en
`ggplot2` de algunos gráficos presentados por Cleveland en su libro
“Visualizing Data”. Ha sido creado para ser usado en cursos de Análisis
Exploratorio de Datos. Se encuentra en desarrollo y aún no ha sido usado
con diversos conjuntos de datos. Cualquier comentario o sugerencia es
bienvenida.

This package provides functions to produce ggplot versions for some
visualizations tools described in Cleveland’s book “Visualizing Data”.
It is an experimental package, and was thought to be used in the context
of a course on exploratory data analysis. Any contributions or feedback
are appreciated.

## Instalación / Instalation

<!-- 
You can install the released version of karel from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("karel")
```
-->

Se puede instalar la versión en desarrollo del paquete `ggcleveland`
desde [GitHub](https://github.com/mpru/ggcleveland) con:

``` r
# install.packages("devtools")
devtools::install_github("mpru/ggcleveland")
```

## Ejemplos / Examples

### gg\_quantiles

La función `gg_quantiles()` produce gráficos QQ (cuantil-cuantil) para
comparar la distribución de una variable cuantitativa en dos o más
grupos:

``` r
# Paquetes, datos y configuraciones
library(ggcleveland)
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
theme_set(theme_bw() + theme(panel.spacing = unit(0, "lines")))
data("futbol")

# Dos grupos
futbol2 <- dplyr::filter(futbol, longp %in% c("< 0.81 m", "0.81 a 0.90 m"))
gg_quantiles(futbol2, dist, longp)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="70%" style="display: block; margin: auto;" />

``` r
# Más de dos grupos
gg_quantiles(futbol, dist, longp, size = 0.4, color = "red", shape = 3) +
    labs(title = "Gráficos QQ de a pares", x = "Distancia (m)", y = "Distancia (m)")
#> Warning: Use of `dataTexto$varX` is discouraged. Use `varX` instead.
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="70%" style="display: block; margin: auto;" />

También permite graficar los cuantiles de cada grupos vs cuantiles
calculados con todos los grupos combinados. Esto puede ser útil para
estudiar residuos luego de algún ajuste:

``` r
futbol <- 
  futbol %>% 
  group_by(longp) %>% 
  mutate(ajuste = mean(dist), res = dist - ajuste)

gg_quantiles(futbol, res, longp, combined = TRUE) 
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="70%" style="display: block; margin: auto;" />

### gg\_tmd

Las funciones `gg_tmd()` y `gg_tmd_paired()` producen el gráfico de
medias-diferencias de Tukey (Tukey’s Mean-Difference Plot):

``` r
# Dos grupos
gg_tmd(futbol2, dist, longp)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="70%" style="display: block; margin: auto;" />

``` r
# Múltiples grupos
gg_tmd(futbol, dist, longp, size = 0.5)
#> Warning: Use of `dataTexto$varX` is discouraged. Use `varX` instead.
```

<img src="man/figures/README-unnamed-chunk-4-2.png" width="70%" style="display: block; margin: auto;" />

``` r
# Datos apareados
gg_tmd_paired(ozone, stamford, yonkers)
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-4-3.png" width="70%" style="display: block; margin: auto;" />

### gg\_rf

La función `gg_rf()` produce un *residual-fit plot* (gráfico de residuos
y valores ajustados):

``` r
gg_rf(futbol, dist, ajuste, res, ylabel = "Distancia (m)")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="70%" style="display: block; margin: auto;" />

``` r
# Agregando las observaciones centradas por la media general
gg_rf(futbol, dist, ajuste, res, cen_obs = TRUE, ylabel = "Distancia (m)")
```

<img src="man/figures/README-unnamed-chunk-5-2.png" width="70%" style="display: block; margin: auto;" />

### gg\_sl

La función `gg_sl()` produce un *spread-location plot*:

``` r
gg_sl(futbol2, dist, longp, xlabel = "Mediana de distancia jittered (m)")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="70%" style="display: block; margin: auto;" />

### gg\_pt

La función `gg_pt()` produce un gráfico para evaluar transformaciones de
potencia, que consisten en elevar a las observaciones a un conjunto de
potencias señaladas en el argumento `taus`, en el cual el valor 0 indica
la transformación logarítmica:

``` r
gg_pt(futbol2, dist, taus = c(-1, -0.5, 0, 0.5))
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="70%" style="display: block; margin: auto;" />

``` r
# Para cada grupo por separado
gg_pt(futbol2, dist, longp, taus = c(-1, -0.5, 0, 0.5))
```

<img src="man/figures/README-unnamed-chunk-7-2.png" width="70%" style="display: block; margin: auto;" />

### gg\_coplot

La función `gg_coplot()` produce *conditioning plots* o *coplots*, los
cuales son una herramienta de visualización útil para estudiar cómo una
variable respuesta depende de 2 o más factores. Un **coplot** permite
visualizar dependencia condicional.

``` r
# Slicing con intervalos solapados
gg_coplot(rubber, x = tensile.strength, y = abrasion.loss, faceting = hardness,
  number_bins = 6, overlap = 3/4,
  ylabel = "Pérdida de abrasión (g/hp-hour))",
  xlabel = "Resistencia a la tracción (kg/cm2)",
  facet_label = "Dureza (grados Shore)", 
  loess_family = "symmetric", size = 2)
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="70%" style="display: block; margin: auto;" />

``` r
# Slicing con intervalos sin solapamientos, con igual amplitud
gg_coplot(rubber, x = tensile.strength, y = abrasion.loss, faceting = hardness,
  number_bins = 6, overlap = 0,
  ylabel = "Pérdida de abrasión (g/hp-hour))",
  xlabel = "Resistencia a la tracción (kg/cm2)",
  facet_label = "Dureza (grados Shore)", 
  loess = FALSE, size = 2)
```

<img src="man/figures/README-unnamed-chunk-8-2.png" width="70%" style="display: block; margin: auto;" />

``` r
# Slicing con intervalos sin solapamientos, con aprox. igual cantidad de datos
gg_coplot(rubber, x = tensile.strength, y = abrasion.loss, faceting = hardness,
  number_bins = 6, overlap = 0, equal_length = F,
  ylabel = "Pérdida de abrasión (g/hp-hour))",
  xlabel = "Resistencia a la tracción (kg/cm2)",
  facet_label = "Dureza (grados Shore)", 
  loess = FALSE, size = 2)
```

<img src="man/figures/README-unnamed-chunk-8-3.png" width="70%" style="display: block; margin: auto;" />

``` r
# Slicing con los valores únicos de la variable de faceting
gg_coplot(galaxy, x = posicion.radial, y = velocidad,
  faceting = angulo, number_bins = 7, loess_span = .5, loess_degree = 2,
  facet_labeller = function(x) paste0("Ángulo = ", x, "º"),
  facet_label = "Ángulo (grado)", facets_nrow = 2, intervals_height = 0.2,
  xlabel = "Posición radial (arcsec)", ylabel = "Velocidad (km/s)")
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-8-4.png" width="70%" style="display: block; margin: auto;" />
