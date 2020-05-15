#' The gg_rf function
#'
#' Returns a Residual-Fit plot, optionally including centered observed values
#'
#' @param df dataframe
#' @param vble numeric variable in df with the observed values
#' @param fitted numeric variable in df with the fitted values
#' @param res numeric variable in df with the residuals
#' @param cen_obs should centered observed values be included in a panel of
#'   their own? Defaults to FALSE. If TRUE, values are centered using the mean
#'   of all data
#' @param cen_obs_label label for the panel of centered observed values
#' @param cen_fit_label label for the panel of fitted values
#' @param res_label label for the panel of residuals
#' @param xlabel x-axis label
#' @param ylabel y-axis label
#' @param ... parameters to be passed to stat_qq(), such as size, color, shape.
#'
#' @return a ggplot
#' @details The option to include the centered observed values as part of this
#'   plot was inspired by work done by Eng. German Beltzer in lattice.
#' @export
#'
#' @examples
#'
#' datos <- dplyr::group_by(futbol, longp)
#' datos <- dplyr::mutate(datos, ajuste = mean(dist), res = dist - ajuste)
#' gg_rf(datos, dist, ajuste, res)
#' gg_rf(datos, dist, ajuste, res, cen_obs = TRUE)
#' gg_rf(datos, dist, ajuste, res, cen_obs = TRUE,
#'       cen_obs_label = "Obs centradas", cen_fit_label = "Ajustados menos media",
#'       res_label = "Residuos", xlabel = "valor f", ylabel = "Distancia (m)",
#'       color = "red", size = 0.7)
#'
gg_rf <- function(df, vble, fitted, res,
									cen_obs = FALSE,
									cen_obs_label = "Centered observed values",
									cen_fit_label = "Centered fitted values",
									res_label = "Residuals",
									xlabel = expression(f[i]),
									ylabel = quo_text(vble),
									...) {

	# NSE y controles
	if (!is.data.frame(df)) stop("The object provided in the argument df is not a data.frame")
	vble <- enquo(vble)
	fitted <- enquo(fitted)
	res <- enquo(res)
	if (!is.numeric(eval_tidy(vble, df)))
		stop(paste(quo_text(vble), "provided for the vble argument is not a numeric variable"))
	if (!is.numeric(eval_tidy(fitted, df)))
		stop(paste(quo_text(fitted), "provided for the fitted argument is not a numeric variable"))
	if (!is.numeric(eval_tidy(res, df)))
		stop(paste(quo_text(res), "provided for the res argument is not a numeric variable"))
	if (!is.logical(cen_obs)) stop("Argument cen_obs must be either TRUE or FALSE")

	df <-
		df %>%
		ungroup() %>% # to get general mean
		mutate(
			centered_observed_values = !!vble - mean(!!vble),
			centered_fitted_values = !!fitted - mean(!!fitted)
		) %>%
		tidyr::pivot_longer(
			cols = c(.data$centered_observed_values, .data$centered_fitted_values, !!res),
			names_to = "tipo", values_to = "valor"
		) %>%
		mutate(tipo = factor(.data$tipo,
												 levels = c("centered_observed_values", "centered_fitted_values", quo_text(res)),
												 labels = c(cen_obs_label, cen_fit_label, res_label))
		)
	# Choose to include or not centered observed values
	if (!cen_obs) {
		df <- filter(df, .data$tipo != cen_obs_label)
	}
	g <- ggplot(df, aes(sample = .data$valor)) +
		stat_qq(distribution = qunif, ...) +
		facet_wrap(~ .data$tipo) +
		xlab(xlabel) + ylab(ylabel)

	return(g)

}

