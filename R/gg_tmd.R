#' The gg_tmd function
#'
#' Returns Tukey's Mean-Difference plot.
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
			geom_text(data = dataTexto, mapping = aes(y = 0, x = mean(range(rtdo$media)), label = dataTexto$varX))
	}
	g <- g + xlab(xlabel) + ylab(ylabel)
	return(g)
}

