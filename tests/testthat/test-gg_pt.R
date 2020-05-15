test_that("Wrong input arguments are detected", {
	data("futbol")
	expect_error(gg_pt(1:10, distancia, longp), "The object provided in the argument df is not a data.frame")
	expect_error(gg_pt(futbol, longp, longp), "longp provided for the vble argument is not a numeric variable")
	expect_error(gg_pt(futbol, dist, dist), "dist provided for the group argument is neither a character nor a factor variable")
	expect_error(gg_pt(futbol, dist, taus = "ḧola"), "taus must be a numeric vector")
	expect_error(gg_pt(futbol, distancia), "object 'distancia' not found")
	expect_error(gg_pt(futbol, dist, nrow = "hola"), "nrow must be a number")
	expect_error(gg_pt(futbol, dist, nrow = -1), "Argument nrow must be a numeric value of length one, greater or equal to 1")
})

test_that("All the code runs ok", {
	data("futbol")
	futbol2 <- futbol[futbol$longp %in% c("< 0.81 m", "0.81 a 0.90 m"), ]
	expect_true(ggplot2::is.ggplot(gg_pt(futbol2, dist, longp)))
	expect_true(ggplot2::is.ggplot(gg_pt(futbol, dist)))
})
