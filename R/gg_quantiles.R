#' The gg_quantiles function
#'
#' Returns a quantile-quantile plot to compare any given number of groups
#'
#' @param df dataframe
#' @param vble numeric variable to be analized
#' @param group character or factor grouping variable
#' @param combined logical, defaults to FALSE, producing a matrix of pairwise QQ
#'   plots. If TRUE, it produces a QQ plot of quantiles of each group versus
#'   quantiles calculated by the combination of all groups. This is useful to
#'   study residuals from a fit.
#' @param ... parameters to be passed to geom_point(), such as size, color, shape.
#'
#' @return a ggplot
#' @export
#'
#' @importFrom dplyr ungroup pull group_by arrange mutate row_number n vars filter as_tibble rename bind_rows tibble summarise starts_with
#' @importFrom stats quantile qqplot median qunif setNames
#' @importFrom ggplot2 ggplot aes xlab ylab coord_fixed facet_grid geom_text geom_jitter geom_line geom_point geom_abline facet_wrap geom_hline stat_qq stat_qq_line
#' @importFrom rlang .data quo_text quo_is_null
#'
#' @examples
#' data(futbol)
#' # Multiple groups
#' gg_quantiles(futbol, dist, longp)
#' gg_quantiles(futbol, dist, longp, size = 0.4, color = "red", shape = 3)
#'
#' # Only 2 grupos
#' futbol2 <- dplyr::filter(futbol, longp %in% c("< 0.81 m", "0.81 a 0.90 m"))
#' gg_quantiles(futbol2, dist, longp)
#'
#' # Each groups vs quantiles from all groups combined
#' gg_quantiles(futbol, dist, longp, combined = TRUE)
gg_quantiles <- function(df, vble, group, combined = FALSE, ...) {

	# NSE
	vble <- enquo(vble)
	group <- enquo(group)

	# Controles
	df <- ungroup(df) # si esta agrupada no deja modificar esa vble

	# if (!is.numeric(pull(df, vble))) stop(paste(vble, "no es numerica"))
	# if (is.numeric(pull(df, group))) mutate(df, !!sym(group) := as.character(pull(df, group))) # para crear una columna cuyo nombre fue pasado como caracter en group

	# Identificar los grupos
	grupos <- unique(pull(df, !!group))
	if (length(grupos) < 2) stop("There's only one group")

	if (combined) {
		# Grafico de cuantiles de cada grupo vs cuantiles combinados, en paneles (diapo 37)
		df <-
			df %>%
			group_by(!!group)  %>%
			arrange(!!vble)  %>%
			mutate(valorf = (row_number() - 0.5) / n())  %>%
			# desagrupar para que no siga haciendo cálculos para cada grupo de longp
			ungroup() %>%
			# con todos los datos, calcular el cuantil que acumula la misma probabilidad que cada dato
			mutate(cuantilComb = quantile(!!vble, .data$valorf))

		g <- ggplot(df, aes(y = !!vble, x = .data$cuantilComb)) +
			geom_point(...) +
			geom_abline(intercept = 0, slope = 1) +
			facet_wrap(vars(!!group)) +
			xlab("Cuantiles combinados") + ylab("Cuantiles de cada grupo")

	} else if (length(grupos) == 2) {
		# Dos grupos, un solo grafico de cuantiles (diapo 13)
		x <- df %>% filter(!!group == grupos[1]) %>% pull(!!vble)
		y <- df %>% filter(!!group == grupos[2]) %>% pull(!!vble)

		g <- qqplot(x = x, y = y, plot.it = F) %>%
			as_tibble() %>%
			ggplot(aes(x = x, y = y)) +
			geom_point(...) +
			geom_abline(intercept = 0, slope = 1) +
			coord_fixed() +
			xlab(grupos[1]) + ylab(grupos[2])
	} else {
		# Mas de dos grupos, matriz de scatterplots de graficos de cuantiles (diapo 18)

		# Grilla con las combinaciones de los grupos, saco los que son iguales (la diagonal)
		grilla <-
			expand.grid(grupos, grupos, stringsAsFactors = F) %>%
			rename(grupoX = .data$Var1, grupoY = .data$Var2) %>%
			filter(.data$grupoX != .data$grupoY)

		# Funcion auxiliar para mapply, dados los nombres de dos grupos genera los
		# cuantiles a graficar gx e gy son los nombres de los grupos, devuelve un
		# tibble
		aux <- function(gx, gy) {
			x <- df %>% filter(!!group == gx) %>% pull(!!vble)
			y <- df %>% filter(!!group == gy) %>% pull(!!vble)
			dat <- qqplot(x, y, plot.it = F) %>% as_tibble()
			dat$varX <- gx
			dat$varY <- gy
			return(dat)
		}

		# Para cada combinacion de grupos aplicar la funcion anterior y combinar los
		# resultantes tibbles
		rtdo <- mapply(aux, grilla$grupoX, grilla$grupoY, SIMPLIFY = F) %>% bind_rows()

		# Hago un dataset para agregar como texto los nombres de las variables en los
		# paneles de la diagonal
		dataTexto <- tibble(varX = grupos, varY = grupos)

		# Grafico
		g <- ggplot(rtdo, aes(x = x, y = y)) +
			geom_point(...) +
			geom_abline(aes(intercept = 0, slope = 1)) +
			facet_grid(varY ~ varX) +
			geom_text(data = dataTexto,
								mapping = aes(y = mean(range(rtdo$y)), x = mean(range(rtdo$x)),
															label = dataTexto$varX)) +
			xlab(vble) + ylab(vble)
	}
	return(g)
}