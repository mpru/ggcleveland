test_that("Wrong input arguments are detected", {
	data("futbol")
	datos <- dplyr::group_by(futbol, longp)
	datos <- dplyr::mutate(datos, ajuste = mean(dist), res = dist - ajuste)
	expect_error(gg_rf(1:10, dist, ajuste, res), "The object provided in the argument df is not a data.frame")
	expect_error(gg_rf(datos, longp, ajuste, res), "longp provided for the vble argument is not a numeric variable")
	expect_error(gg_rf(datos, dist, longp, res), "longp provided for the fitted argument is not a numeric variable")
	expect_error(gg_rf(datos, dist, ajuste, longp), "longp provided for the res argument is not a numeric variable")
	expect_error(gg_rf(datos, dist, ajuste, res, 1), "Argument cen_obs must be either TRUE or FALSE")
})

test_that("All the code runs ok", {
	data("futbol")
	datos <- dplyr::group_by(futbol, longp)
	datos <- dplyr::mutate(datos, ajuste = mean(dist), res = dist - ajuste)
	expect_true(ggplot2::is.ggplot(gg_rf(datos, dist, ajuste, res)))
	expect_true(ggplot2::is.ggplot(gg_rf(datos, dist, ajuste, res, cen_obs = TRUE)))
})
