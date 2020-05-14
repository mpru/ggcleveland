#' The gg_pt function
#'
#' Returns of normal QQ plots for a set of power transformations. If there are
#' groups in the data, transformations can be applied separately to each of
#' them.
#'
#' @param df dataframe
#' @param vble numeric variable in df to be transformed
#' @param group optional character or factor grouping variable in df. Defaults
#'   to NULL.
#' @param taus vector of numeric values for the power transformations (0 is
#'   considered to be the log transform)
#' @param xlabel x-axis label
#' @param ylabel y-axis label
#' @param nrow number of rows for facet_wrap, only applied when group is NULL.
#' @param ... parameters to be passed to stat_qq(), such as size, color, shape.
#'
#' @returna a ggplot
#' @export
#'
#' @examples
#' # Without groups
#' data(fusion)
#' gg_pt(dplyr::filter(fusion, nv.vv == "VV"), time)
#' gg_pt(dplyr::filter(fusion, nv.vv == "VV"), time, taus = c(-0.25, -0.5, -1, 0),
#'       xlabel = "Cuantiles normales", ylabel = "Valores transformados",
#'       nrow = 3, color = "red")
#'
#' # With groups
#' gg_pt(fusion, time, nv.vv)
#'
gg_pt <- function(df, vble, group = NULL,
									taus = c(-1, -.5, -.25, 0, .25, .5, 1),
									xlabel = "Normal quantiles",
									ylabel = paste("Transformed", quo_text(vble)),
									nrow = 2,...) {

	# Agregar controles

	# NSE
	vble <- enquo(vble)
	group <- enquo(group)

	# Transformar valores
	datos_pot <-
		sapply(taus, transf_pot, x = pull(df, !!vble)) %>%
		as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
		setNames(taus)
	if (quo_is_null(group)) {
		datos_pot <- tidyr::pivot_longer(datos_pot, cols = dplyr::everything(), names_to = "tau", values_to = "y")
	} else {
		datos_pot <-
			datos_pot %>%
			mutate(groups = pull(df, !!group)) %>%
			tidyr::pivot_longer(cols = -starts_with("groups"), names_to = "tau", values_to = "y")
	}

	# Graficar
	g <- ggplot(datos_pot, aes(sample = .data$y)) +
		stat_qq(...) +
		stat_qq_line() +
		xlab(xlabel) + ylab(ylabel)

	if (quo_is_null(group)) {
		g <- g + facet_wrap(~ tau, scales = "free_y", nrow = nrow)
	} else {
		g <- g + facet_wrap(groups ~ tau, scales = "free_y", nrow = 2)
	}
	return(g)
}


#' The trasf_pot function
#'
#' Helper function for gg_pt
#'
#' @param x numeric vector to be transformed
#' @param tau powers
#'
#' @return vector of transformed values
#'
#' @keywords internal
transf_pot <- function(x, tau = 0) {
	if(tau == 0) {
		x = log(x)
	} else {
		x = x^tau
	}
	return(x)
}

# Con facet_grid quedan mejor los paneles pero no puedo liberar el eje Y
# ggplot(datos_pot, aes(sample = y)) +
#     stat_qq() +
#     stat_qq_line() +
#     facet_grid(nv.vv ~ tau, scales = "free_y") +
#     xlab("Cuantiles normales") + ylab("Tiempo VV transformado")


