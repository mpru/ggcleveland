test_that("Wrong input arguments are detected", {
	data("futbol")
	expect_error(gg_sl(1:10, distancia, longp), "The object provided in the argument df is not a data.frame")
	expect_error(gg_sl(futbol, longp, longp), "longp provided for the vble argument is not a numeric variable")
	expect_error(gg_sl(futbol, dist, dist), "dist provided for the group argument is neither a character nor a factor variable")
	expect_error(gg_sl(futbol, distancia, longp), "object 'distancia' not found")
	futbol$algo <- "a"
	expect_error(gg_sl(futbol, dist, algo), "There's only one group")
})

test_that("All the code runs ok", {
	data("futbol")
	expect_true(ggplot2::is.ggplot(gg_sl(futbol, dist, longp)))
})
