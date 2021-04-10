#' The equal_count function
#'
#' This function applies the equal count algorithm to divide a set of observations into intervals which can have certain level of ovelapping. It calls `lattice::equal.count` but extends the output.
#'
#' @param df dataframe
#' @param vble numeric variable to be analized
#' @param n_int number of intervals
#' @param frac overlapping fraction
#'
#' @return a list with two elements:
#' \describe{
#'   \item{intervals}{a tibble where each rows referes to one of the generated interval, with its lower and upper limits, number of values in it and number of values overlapping with the next interval}
#'   \item{df_long}{a tibble in long format where each observation appears as many times as the number of intervals in which it belongs, with an identifier of the observation (`id`, its position in the original data.frame) and an identifier of the interval.}
#'   }
#' @export
#'
#' @examples
#' equal_count(iris, Sepal.Length, 15, 0.3)
equal_count <- function(df, vble, n_int = 6, frac = 0.5) {

	# NSE y controles
	if (!is.data.frame(df)) stop("The object provided in the argument df is not a data.frame")
	vble <- enquo(vble)
	if (!is.numeric(eval_tidy(vble, df)))
		stop(paste(quo_text(vble), "provided for the vble argument is not a numeric variable"))
	if (!is.numeric(n_int) || n_int < 0)
		stop("n_int should be numeric equal or greater than 0")
	if (!is.numeric(frac) || frac < 0)
		stop("frac should be numeric equal or less than 1")

	# Uso la función equal.count de lattice que lo implementa, no encontré
	# otra cosa en ggplot o tidyverse para esto

	# res es un tibble con una fila por intervalo
	valores <- pull(df, !!vble)
	res <-
		lattice::equal.count(valores, n_int, frac) %>%
		levels() %>%
		sapply(function(x) x) %>%
		t() %>%
		`colnames<-`(c("lower", "upper")) %>%
		tibble::as_tibble() %>%
		dplyr::mutate(n = dplyr::row_number()) %>%
		dplyr::select(.data$n, .data$lower, .data$upper)

	# Para cada intervalo agrego una columna al dataset indicando si los valores de
	# x están o no ahí adentro
	for (i in 1:n_int) {
		lims <- as.numeric(res[i, 2:3])
		df <- dplyr::mutate(df, !!paste0("int", i) := cut(valores, breaks = lims, labels = i))
	}

	# Genero un dataset en formato largo, cada valor se repite tantas veces como
	# nro de intervalos en los que esté presente. Esto sirve para graficar.
	df_long <-
		df %>%
		dplyr::mutate(id = dplyr::row_number()) %>%
		tidyr::pivot_longer(cols = dplyr::starts_with("int"),
												names_to = "nombre", values_to = "interval") %>%
		dplyr::select(-.data$nombre) %>%
		dplyr::mutate(interval = factor(as.numeric(.data$interval))) %>%
		dplyr::filter(!is.na(.data$interval))

	# Conteo de observaciones en cada intervalo
	res$count <- table(df_long$interval)

	# Overlap entre cada intervalo y el siguiente
	overlap <- rep(NA, n_int)
	df_split <- split(df_long, df_long$interval)
	for (i in 1:(n_int - 1)) {
		overlap[i] <- length(intersect(df_split[[i]]$id, df_split[[i + 1]]$id))
	}
	res$overlap <- overlap

	return(list(intervals = res, df_long = df_long))
}

