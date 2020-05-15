#' The gg_sl function
#'
#' Returns a spread-location plot.
#'
#' @param df dataframe
#' @param vble numeric variable to be analized
#' @param group grouping character or factor variable
#' @param jitterwidth width argument for geom_jitter
#' @param jitteralpha alpha argument for geom_jitter
#' @param linecol col argument for geom_line
#' @param ylabel Y-axis label
#'
#' @return a ggplot object with the spread-location plot
#' @importFrom magrittr %>%
#' @importFrom dplyr enquo
#' @export
#' @examples
#' data(fusion)
#' gg_sl(fusion, time, nv.vv)
#' gg_sl(fusion, time, nv.vv, jitterwidth = 0.4, linecol = "blue",
#'       ylabel = "hola", jitteralpha = 1) +
#'       ggplot2::scale_color_discrete("Grupo")
#'
gg_sl <- function(df, vble, group,
									jitterwidth = 0.1, jitteralpha = 0.5, linecol = "red",
									ylabel = expression(sqrt(abs( " Residuos "))),
									xlabel = "Medians") {

	# NSE y controles
	if (!is.data.frame(df)) stop("The object provided in the argument df is not a data.frame")
	vble <- enquo(vble)
	group <- enquo(group)
	if (!is.numeric(eval_tidy(vble, df)))
		stop(paste(quo_text(vble), "provided for the vble argument is not a numeric variable"))
	if (!is.character(eval_tidy(group, df)) && !is.factor(eval_tidy(group, df)))
		stop(paste(quo_text(group), "provided for the group argument is neither a character nor a factor variable"))

	if (length(unique(pull(df, !!group))) < 2) stop("There's only one group")

	# Calcular mediana y residuos
	df1 <-
		df %>%
		group_by(!!group) %>%
		mutate(
			mna = median(!!vble),
			mna_res = sqrt(abs(!!vble - .data$mna))
		)

	# Preparar otro data set para unir las medianas de los residuos
	datos_linea <-
		df1 %>%
		group_by(!!group) %>%
		summarise(x = median(!!vble), y = median(.data$mna_res))

	# Gráfico s-l
	g <- ggplot(df1, aes(x = .data$mna, y = .data$mna_res, color = !!group)) +
					geom_jitter(alpha = jitteralpha, width = jitterwidth, height = 0) +
					geom_line(data = datos_linea, aes(x = .data$x, y = .data$y), col = linecol) +
					ylab(ylabel) + xlab(xlabel)

	return(g)
}
