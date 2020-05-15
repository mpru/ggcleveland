test_that("Wrong input arguments are detected", {
	data("futbol")
	expect_error(gg_quantiles(1:10, distancia, longp), "The object provided in the argument df is not a data.frame")
	expect_error(gg_quantiles(futbol, longp, longp), "longp provided for the vble argument is not a numeric variable")
	expect_error(gg_quantiles(futbol, dist, dist), "dist provided for the group argument is neither a character nor a factor variable")
	expect_error(gg_quantiles(futbol, dist, longp, 1), "Argument combined must be either TRUE or FALSE")
	expect_error(gg_quantiles(futbol, distancia, longp), "object 'distancia' not found")
	futbol$var <- "a"
	expect_error(gg_quantiles(futbol, dist, var), "There's only one group")
})

test_that("All the code runs ok", {
	data("futbol")
	expect_true(ggplot2::is.ggplot(gg_quantiles(futbol, dist, longp, combined = TRUE)))
	expect_true(ggplot2::is.ggplot(gg_quantiles(futbol, dist, longp)))
	futbol2 <- futbol[futbol$longp %in% c("< 0.81 m", "0.81 a 0.90 m"), ]
	expect_true(ggplot2::is.ggplot(gg_quantiles(futbol2, dist, longp)))
})
