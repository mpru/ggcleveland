# Función que crea datasets listos para graficar coplots con el algoritmo de equal count
#' The make_coplot_df function
#'
#' It creates dataframes to be used in coplot
#'
#' @param df dataframe
#' @param vble faceting numeric variable
#' @param number_bins integer; the number of conditioning intervals
#' @param overlap numeric < 1; the fraction of overlap of the conditioning variables
#'
#' @return a dataset to be used in the creation of coplots
#' @details Adapted from \href{https://jfukuyama.github.io/teaching/stat670/notes/lecture-11.html}{here}.
#' @export
#'
#' @examples
#' data(rubber)
#' data_coplot <- make_coplot_df(rubber, hardness, 6, 3/4)
make_coplot_df <- function(df, vble, number_bins = 6, overlap = 0.5) {

	# Obs: con overlap = 0 co.intervals a veces genera algo de superposicion de
	# todos modos. Si quiero evitar esto debería hacer algún cambio, tal vez usando
	# cut_number de ggplot2. `ggplot2` trae algunas funciones que son útiles para
	# agrupar datos en intervalos, como `cut_interval()`, `cut_number()`,
	# `cut_width()`.

	# NSE y controles
	if (!is.data.frame(df)) stop("The object provided in the argument df is not a data.frame")
	vble <- enquo(vble)
	if (!is.numeric(eval_tidy(vble, df)))
		stop(paste(quo_text(vble), "provided for the vble argument is not a numeric variable"))

	values <- pull(df, !!vble)

	## co.intervals gets the limits used for the conditioning intervals
	intervals = graphics::co.intervals(values, number = number_bins, overlap = overlap)
	## indices is a list, with the ith element containing the indices of the
	## observations falling into the ith interval
	indices = indices = lapply(split(intervals, seq(nrow(intervals))),
														 function(x) which(values <= x[2] & values >= x[1]))
	## interval_descriptions is formatted like indices, but has interval
	## names instead of indices of the samples falling in the index
	interval_descriptions = apply(intervals, 1, function(x) {
		num_in_interval = sum(values <= x[2] & values >= x[1])
		interval_description = sprintf("[%.2f, %.2f]", x[1], x[2])
		return(rep(interval_description, num_in_interval))
	})
	## df_expanded has all the points we need for each interval, and the
	## 'interval' column tells us which part of the coplot the point should
	## be plotted in
	df_expanded = df[unlist(indices),]
	df_expanded$interval = factor(unlist(interval_descriptions),
																levels = unique(unlist(interval_descriptions)),
																labels = paste(quo_text(vble), "=", unique(unlist(interval_descriptions))),
																ordered = TRUE)
	# df_expanded <-
	# 	df_expanded %>%
	# 	separate(interval, c("li", "ls"), sep = ",") %>%
	# 	mutate(n = dplyr::row_number(), li = readr::parse_number(.data$li), ls = readr::parse_number(.data$ls)) %>%

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
#' @param loess logical; should a loess smoothing curve be added to the coplots?
#'   Defaults to TRUE.
#' @param loess_span span parameter for loess
#' @param loess_degree degree parameter for loess
#' @param loess_family famiyly argument for the loess() function
#' @param ylabel label for y-axis
#' @param xlabel label for x-axis
#' @param facetlabel label for faceting variable
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
#' @export
#'
#' @examples
#' data(rubber)
#' gg_coplot(rubber, x = tensile.strength, y = abrasion.loss, faceting = hardness,
#'   number_bins = 6, overlap = 3/4,
#'   ylabel = "Pérdida de abrasión (g/hp-hour))",
#'   xlabel = "Resistencia a la tracción (kg/cm2)",
#'   facetlabel = "Dureza (grados Shore)", loess_family = "symmetric", size = 2)
#'
gg_coplot <- function(df, x, y, faceting, number_bins = 6, overlap = 0.5,
											loess = TRUE, loess_span = 3/4, loess_degree = 1, loess_family = "gaussian",
											ylabel = quo_text(x), xlabel = quo_text(y), facetlabel = quo_text(faceting),
											show_intervals = TRUE, intervals_height = 0.25,
											remove_strip = FALSE, facets_nrow = NULL, hline_at = NULL, ...) {

	# Obs: con overlap = 0 co.intervals a veces genera algo de superposicion de
	# todos modos. Si quiero evitar esto debería hacer algún cambio, tal vez usando
	# cut_number de ggplot2. `ggplot2` trae algunas funciones que son útiles para
	# agrupar datos en intervalos, como `cut_interval()`, `cut_number()`,
	# `cut_width()`.

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

	res_make_coplot_df <- make_coplot_df(df, !!faceting, number_bins, overlap)
	data_coplot <- res_make_coplot_df[[1]]

	g1 <- ggplot(data_coplot, aes(x = !!x, y = !!y)) +
		geom_point(...) +
		facet_wrap(~ interval, nrow = facets_nrow) +
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
		g2 <-
			ggplot(res_make_coplot_df[[2]]) +
				geom_rect(aes(xmin = .data$li, xmax = .data$ls, ymin = .data$n - 0.25, ymax = .data$n + 0.25)) +
				labs(x = facetlabel) +
				theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

		egg::ggarrange(g2, g1, heights = c(intervals_height, 1))
	} else {
		return(g1)
	}
}

