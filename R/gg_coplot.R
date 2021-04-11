# Función que crea datasets listos para graficar coplots con el algoritmo de equal count
#' The make_coplot_df function
#'
#' It creates dataframes to be used in coplot
#'
#' @param df dataframe
#' @param vble faceting numeric variable
#' @param number_bins integer; the number of conditioning intervals
#' @param overlap numeric < 1; the fraction of overlap of the conditioning variables
#' @param equal_length if `overlap = 0` non overlaping intervals are produced
#'   all with same length if `equal_length` is `TRUE` (default) or with the same
#'   number of values otherwise.
#'
#' @return a dataset to be used in the creation of coplots
#' @details Adapted from \href{https://jfukuyama.github.io/teaching/stat670/notes/lecture-11.html}{here}.
#' @export
#'
#' @details If `overlap = 0` then `ggplot2::cut_interval` is used to generate
#'   the intervals if `equal_length = TRUE` (default), otherwise
#'   `ggplot2::cut_number` is used. If `overlap` is not zero,
#'   `graphics::co.interval` is called.
#'
#' @examples
#' data_coplot <- make_coplot_df(rubber, hardness, 6, 3/4)
make_coplot_df <- function(df, vble, number_bins = 6, overlap = 0.5, equal_length = TRUE) {

		# NSE y controles
	if (!is.data.frame(df)) stop("The object provided in the argument df is not a data.frame")
	vble <- enquo(vble)
	if (!is.numeric(eval_tidy(vble, df)))
		stop(paste(quo_text(vble), "provided for the vble argument is not a numeric variable"))

	values <- pull(df, !!vble)

	if (overlap == 0) {
		# Obs: con overlap = 0 co.intervals a veces genera algo de superposicion de
		# todos modos. Si quiero evitar esto debería hacer algún cambio, tal vez
		# usando cut_number de ggplot2. `ggplot2` trae algunas funciones que son
		# útiles para agrupar datos en intervalos, como `cut_interval()`,
		# `cut_number()`, `cut_width()`. cut_number() makes n groups with
		# (approximately) equal numbers of observations;
		# Nueva obs: no sé por qué aclaré lo anterior, no  veo que co.intervals genere
		# superposición si overlap = 0, pero el tema es que lo que devuelve es el
		# rango dentro de cada intervalo, o sea no te da puntos de cortes, no quedan
		# intervalos continuos, te da por ejemplo "0.3-0.7", "1.2-1.9" en lugar de
		# "0-1", "1-2". Por eso uso cut_number que sí te lo da, asi puedo representar
		# de forma continua los intervalos, pero el coplot en sí quedaría igual

		if (equal_length) {
			# non overlaping intervals have same length
			cut_values <- ggplot2::cut_interval(values, number_bins)
		} else {
			# non overlaping intervals have same number of points
			cut_values <- ggplot2::cut_number(values, number_bins)
		}

		# usar dig.lab = 6 o lo que sea necesario dentro de cut_number si se quieren
		# ver mas decimales en la definicion de los intervalos

		intervals <-
			cut_values %>%
			levels() %>%
			stringr::str_split(",") %>%
			unlist() %>%
			readr::parse_number() %>%
			matrix(ncol = 2, byrow = T)

		# le agrego el nombre de la variable para generar etiquetas
		levels(cut_values) <- paste(quo_text(vble), "=", levels(cut_values))
		df_expanded <- mutate(df, interval = cut_values)

	} else {

		# Acá sí uso co.intervals
		## co.intervals gets the limits used for the conditioning intervals
		intervals <- graphics::co.intervals(values, number = number_bins, overlap = overlap)
		## indices is a list, with the ith element containing the indices of the
		## observations falling into the ith interval
		indices <- lapply(split(intervals, seq(nrow(intervals))),
											function(x) which(values <= x[2] & values >= x[1]))
		## interval_descriptions is formatted like indices, but has interval
		## names instead of indices of the samples falling in the index
		interval_descriptions <- apply(intervals, 1, function(x) {
			num_in_interval <- sum(values <= x[2] & values >= x[1])
			interval_description <- sprintf("[%.2f, %.2f]", x[1], x[2])
			return(rep(interval_description, num_in_interval))
		})
		## df_expanded has all the points we need for each interval, and the
		## 'interval' column tells us which part of the coplot the point should
		## be plotted in
		df_expanded = df[unlist(indices),]
		df_expanded$interval = factor(unlist(interval_descriptions),
																	levels = unique(unlist(interval_descriptions)),
																	labels = paste(quo_text(vble), "=",
																								 unique(unlist(interval_descriptions))),
																	ordered = TRUE)
		# df_expanded <-
		# 	df_expanded %>%
		# 	separate(interval, c("li", "ls"), sep = ",") %>%
		# 	mutate(n = dplyr::row_number(), li = readr::parse_number(.data$li), ls = readr::parse_number(.data$ls)) %>%
	}
	intervals <- tibble(n = 1:nrow(intervals), li = intervals[, 1], ls = intervals[, 2])
	return(list(df_expanded = df_expanded, intervals = intervals))
}

#' The gg_coplot function
#'
#' Implements conditional plots or coplots.
#'
#' @param df dataframe
#' @param x numeric variable for x-axis
#' @param y numeric variable for y-axis
#' @param faceting faceting numeric variable
#' @param number_bins integer; the number of conditioning intervals
#' @param overlap numeric < 1; the fraction of overlap of the conditioning
#'   variables
#' @param equal_length if `overlap = 0` non overlaping intervals are produced
#'   all with same length if `equal_length` is `TRUE` (default) or with the same
#'   number of values otherwise.
#' @param loess logical; should a loess smoothing curve be added to the coplots?
#'   Defaults to TRUE.
#' @param loess_span span parameter for loess
#' @param loess_degree degree parameter for loess
#' @param loess_family famiyly argument for the loess() function
#' @param ylabel label for y-axis
#' @param xlabel label for x-axis
#' @param facet_label label for faceting variable
#' @param facet_labeller defaults to NULL so facet labels are automatically
#'   produced, but can take a fuction to be used in `facet_wrap(~faceting,
#'   labeller = labeller(faceting = facet_labeler))`
#' @param show_intervals logical; should the overlapping intervals be shown on
#'   their own panel on the top of the figure? Defaults to TRUE.
#' @param intervals_height numeric between 0 and 1, relative size of the
#'   intervals pane
#' @param facets_nrow integer; number of rows for the facets
#' @param remove_strip logical; should de facets have no strips with labels?
#'   Default to FALSE.
#' @param hline_at numeric; if provide a horizontal line will be added at that
#'   heigth
#' @param ... addtional parameters passed to geom_point()
#'
#' @return a coplot
#' @details If the number of bins is equal to the number of unique values in the
#'   faceting variable, then no overlaping intervals are produced and each value
#'   in the faceting variable is used as a slice (`frac` is ingored).
#'
#'   If `overlap = 0` then `ggplot2::cut_interval` is used to generate
#'   the intervals if `equal_length = TRUE` (default), otherwise
#'   `ggplot2::cut_number` is used. If `overlap` is not zero,
#'   `graphics::co.interval` is called.
#' @export
#'
#' @examples
#' # Slicing con intervalos solapados
#' gg_coplot(rubber, x = tensile.strength, y = abrasion.loss, faceting = hardness,
#'   number_bins = 6, overlap = 3/4,
#'   ylabel = "Pérdida de abrasión (g/hp-hour))",
#'   xlabel = "Resistencia a la tracción (kg/cm2)",
#'   facet_label = "Dureza (grados Shore)", loess_family = "symmetric", size = 2)
#'
#' # Slicing con los valores únicos de la variable de faceting
#' gg_coplot(galaxy, x = posicion.radial, y = velocidad,
#'   faceting = angulo, number_bins = 7, loess_span = .5, loess_degree = 2,
#'   facet_labeller = function(x) paste0("Ángulo = ", x, "º"),
#'   facet_label = "Ángulo (grado)", facets_nrow = 2, intervals_height = 0.2,
#'   xlabel = "Posición radial (arcsec)", ylabel = "Velocidad (km/s)")
#'
#' gg_coplot(galaxy, x = este.oeste, y = norte.sur, faceting = velocidad,
#'   number_bins = 25, overlap = 0,  size = 0.5,
#'   ylabel = "Coordenada sur-norte jittered (arcsec)",
#'   xlabel = "Coordenada este-oeste jittered (arcsec)",
#'   facet_label = "Velocidad (km/s)", facets_nrow = 5,
#'   remove_strip = TRUE, intervals_height = 0.15, loess = FALSE)
gg_coplot <- function(df, x, y, faceting, number_bins = 6, overlap = 0.5,
											equal_length = TRUE, loess = TRUE, loess_span = 3/4,
											loess_degree = 1, loess_family = "gaussian",
											ylabel = quo_text(y), xlabel = quo_text(x),
											facet_label = quo_text(faceting), facet_labeller = NULL,
											show_intervals = TRUE, intervals_height = 0.25,
											remove_strip = FALSE, facets_nrow = NULL, hline_at = NULL, ...) {

	# NSE y controles
	if (!is.data.frame(df)) stop("The object provided in the argument df is not a data.frame")
	x <- enquo(x)
	y <- enquo(y)
	faceting <- enquo(faceting)
	if (!is.numeric(eval_tidy(x, df)))
		stop(paste(quo_text(x), "provided for the x argument is not a numeric variable"))
	if (!is.numeric(eval_tidy(y, df)))
		stop(paste(quo_text(y), "provided for the y argument is not a numeric variable"))
	if (!is.numeric(eval_tidy(faceting, df)))
		stop(paste(quo_text(faceting), "provided for the faceting argument is not a numeric variable"))

	unicos <- unique(pull(df, !!faceting))
	if (number_bins == length(unicos)) {

		# si number_bins es igual a la cantidad de datos unicos, entonces no se generan
		# intervalos y se usan esos datos unicos como slices (codigo agregado)
		g1 <- ggplot(df, aes(x = !!x, y = !!y))
		if (is.null(facet_labeller)) {
			g1 <- g1 + facet_wrap(vars(!!faceting), nrow = facets_nrow)
		} else {
			g1 <- g1 + facet_wrap(vars(!!faceting), nrow = facets_nrow,
														labeller = labeller(.rows = facet_labeller))
		}

	} else {

		# Hacer el coplot con slicing (version original)
		res_make_coplot_df <- make_coplot_df(df, !!faceting, number_bins, overlap, equal_length)
		data_coplot <- res_make_coplot_df[[1]]
		g1 <- ggplot(data_coplot, aes(x = !!x, y = !!y))

		if (is.null(facet_labeller)) {
			g1 <- g1 + facet_wrap(~ interval, nrow = facets_nrow)
		} else {
			g1 <- g1 + facet_wrap(~ interval, nrow = facets_nrow,
														labeller = labeller(.rows = facet_labeller))
		}

	}

	# Completar el grafico
	g1 <- g1 +
		geom_point(...) +
		labs(y = ylabel, x = xlabel)

	if (loess) {
		g1 <- g1 +
			stat_smooth(method = "loess", se = FALSE, span = loess_span,
									method.args = list(degree = loess_degree, family = loess_family))
	}

	if (remove_strip) {
		g1 <- g1 + theme(strip.text = element_blank())
	}

	if (!is.null(hline_at)) {
		g1 <- g1 + geom_hline(yintercept = hline_at)
	}

	if (show_intervals) {

		if (number_bins == length(unicos)) {

			# si number_bins es igual a la cantidad de datos unicos, solo muestro puntitos
			g2 <- ggplot(NULL, aes(x = unicos, y = 1)) +
				geom_point(size = 2) +
				scale_x_continuous(facet_label, breaks = unicos) +
				theme(
					axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
					axis.title.y = element_blank(),
					panel.grid = element_blank()
				)

		} else {

			# sino los rectangulos
			g2 <-
				ggplot(res_make_coplot_df[[2]]) +
				geom_rect(aes(xmin = .data$li, xmax = .data$ls, ymin = .data$n - 0.25,
											ymax = .data$n + 0.25)) +
				labs(x = facet_label) +
				theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
		}
		return(egg::ggarrange(g2, g1, heights = c(intervals_height, 1)))
	} else {
		return(g1)
	}
}



