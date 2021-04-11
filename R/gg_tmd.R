#' The gg_tmd function
#'
#' Returns Tukey's Mean-Difference plot for one-way data
#'
#' @param df dataframe
#' @param vble numeric variable to be analized
#' @param group character or factor grouping variable
#' @param xlabel label for x-axis, defaults to "Mean"
#' @param ylabel label for y-axis, defaults to "Difference"
#' @param ... parameters to be passed to geom_point(), such as size, color, shape.
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' data(futbol)
#' # Multiple groups
#' gg_tmd(futbol, dist, longp)
#' gg_tmd(futbol, dist, longp, size = 0.4, color = "red", shape = 3)
#'
#' # Only 2 grupos
#' futbol2 <- dplyr::filter(futbol, longp %in% c("< 0.81 m", "0.81 a 0.90 m"))
#' gg_tmd(futbol2, dist, longp)
gg_tmd <- function(df, vble, group, xlabel = "Mean", ylabel = "Difference", ...) {

	# NSE y controles
	if (!is.data.frame(df)) stop("The object provided in the argument df is not a data.frame")
	vble <- enquo(vble)
	group <- enquo(group)
	if (!is.numeric(eval_tidy(vble, df)))
		stop(paste(quo_text(vble), "provided for the vble argument is not a numeric variable"))
	if (!is.character(eval_tidy(group, df)) && !is.factor(eval_tidy(group, df)))
		stop(paste(quo_text(group), "provided for the group argument is neither a character nor a factor variable"))

	# Identificar los grupos
	grupos <- unique(pull(df, !!group))
	if (length(grupos) < 2) stop("There's only one group")

	if (length(grupos) == 2) {
		# Dos grupos, un solo panel (diapo 16)
		x <- df %>% filter(!!group == grupos[1]) %>% pull(!!vble)
		y <- df %>% filter(!!group == grupos[2]) %>% pull(!!vble)

		g <- qqplot(x, y, plot.it = F) %>%
			as_tibble() %>%
			mutate(resta = .data$y - .data$x, media = (.data$x +  .data$y) / 2) %>%
			ggplot(aes(x = .data$media, y = .data$resta)) +
			geom_point(...) +
			geom_hline(yintercept = 0)

	} else {
		# Mas de dos grupos, generar matriz

		# Grilla con las combinaciones de los grupos, saco los que son iguales (la diagonal)
		grilla <-
			expand.grid(grupos, grupos, stringsAsFactors = F) %>%
			rename(grupoX = .data$Var1, grupoY = .data$Var2) %>%
			filter(.data$grupoX != .data$grupoY)

		# Funcion auxiliar para mapply, dados los nombres de dos grupos genera
		# cuantiles, restas y medias gx e gy son los nombres de los grupos, devuelve
		# un tibble
		aux <- function(gx, gy) {
			x <- df %>% filter(!!group == gx) %>% pull(!!vble)
			y <- df %>% filter(!!group == gy) %>% pull(!!vble)
			dat <-
				qqplot(x, y, plot.it = F) %>%
				as_tibble() %>%
				mutate(resta = .data$y - .data$x, media = (.data$x + .data$y) / 2,
							 varX = gx, varY = gy)
			return(dat)
		}
		# Para cada combinacion de grupos aplicar la funcion anterior y combinar los
		# resultantes tibbles
		rtdo <- mapply(aux, grilla$grupoX, grilla$grupoY, SIMPLIFY = F) %>% bind_rows()

		# Hago un dataset para agregar como texto los nombres de las variables en los
		# paneles de la diagonal
		dataTexto <- tibble(varX = grupos, varY = grupos)

		# Grafico
		g <- ggplot(rtdo, aes(x = .data$media, y = .data$resta)) +
			geom_point(...) +
			geom_hline(aes(yintercept = 0), lty = 2) +
			facet_grid(varY ~ varX) +
			geom_text(data = dataTexto, mapping = aes(y = 0, x = mean(range(rtdo$media)),
																								label = .data$varX))
	}
	g <- g + xlab(xlabel) + ylab(ylabel)
	return(g)
}


#' The gg_tmd_paired function
#'
#' Returns Tukey's Mean-Difference plot for paired data (both variables must be measured in the same scale).
#'
#' @param df dataframe
#' @param vble1,vble2 numeric variables to be analized
#' @param loess logical; should a loess smoothing curve be added to the coplots?
#'   Defaults to TRUE.
#' @param loess_span span parameter for loess
#' @param loess_degree degree parameter for loess
#' @param loess_family famiyly argument for the loess() function
#' @param xlabel label for x-axis, defaults to "Mean"
#' @param ylabel label for y-axis, defaults to "Difference"
#' @param ... parameters to be passed to geom_point(), such as size, color, shape.
#'
#' @details Differences are computed as `vble1 - vble2`
#' @return a ggplot
#' @export
#'
#' @examples
#' gg_tmd_paired(ozone, stamford, yonkers)
gg_tmd_paired <- function(df, vble1, vble2,
													xlabel = "Mean", ylabel = "Difference",
													loess = TRUE, loess_span = 1,
													loess_degree = 1, loess_family = "gaussian", ...) {

	# NSE y controles
	if (!is.data.frame(df)) stop("The object provided in the argument df is not a data.frame")
	vble1 <- enquo(vble1)
	vble2 <- enquo(vble2)
	if (!is.numeric(eval_tidy(vble1, df)))
		stop(paste(quo_text(vble1), "provided for the vble1 argument is not a numeric variable"))
	if (!is.numeric(eval_tidy(vble2, df)))
		stop(paste(quo_text(vble2), "provided for the vble2 argument is not a numeric variable"))

	# Gráfico
	g <-
		df %>%
		mutate(resta = !!vble1 - !!vble2, media = (!!vble1 + !!vble2) / 2) %>%
		ggplot(aes(x = .data$media, y = .data$resta)) +
		geom_point(...) +
		geom_hline(yintercept = 0) +
		xlab(xlabel) + ylab(ylabel)

	if (loess) {
		g <- g +
			stat_smooth(method = "loess", se = FALSE, span = loess_span,
									method.args = list(degree = loess_degree, family = loess_family))
	}

	return(g)
}


