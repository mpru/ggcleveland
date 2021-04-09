#' Dataset futbol
#'
#' Data about leg length and kick distance from 300 football players.
#'
#' @format A data frame with 300 rows and 2 variables:
#' \describe{
#'   \item{longp}{category of leg length}
#'   \item{dist}{kick distance}
#' }
#' @source Unknown
"futbol"

#' Dataset fusion
#'
#' From Cleveland (1993): An experiment was run to study the effect of prior knowledge of an object's form on fusion time when looking at a stereogram. The experimenters measured the time of first fusion for a particular random dot stereogram. There were two groups of subjects. The NV subjects received either no information or verbal information. The VV subjects received a combination of verbal and visual information, either suggestive drawings of the object or a model of it. Thus the VV subjects actually saw something that depicted the object, but the NV subjects did not. The goal in analyzing the fusion times is to determine if there is a shift in the distribution of the VV times toward lower values compared with the NV times.
#'
#' @format A data frame with 78 rows and 2 variables:
#' \describe{
#'   \item{time}{fusion times, seconds}
#'   \item{nv.vv}{experimental group}
#' }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"fusion"

#' Dataset rubber
#'
#' From Cleveland (1993): data from an industrial experiment in which thirty rubber specimens were rubbed by an abrasive material. Measurements of three variables - abrasion loss, hardness, and tensile strength - were made for each specimen. Abrasion loss is the amount of material abraded from a specimen per unit of energy expended in the rubbing; tensile strength is the force per unit of cross-sectional area required to break a specimen; and hardness is the rebound height of a steel indenter dropped onto a specimen. The goal is to determine the dependence of abrasion loss on tensile strength and hardness
#'
#' @format A data frame with 78 rows and 2 variables:
#' \describe{
#'   \item{hardness}{hardness}
#'   \item{tensile.strength}{tensile strength}
#'   \item{abrasion.loss}{abrasion loss}
#'   \item{ts.low}{tensile.strength - 180 if tensile.strength < 180 or 0 otherwise}
#'   \item{ts.high}{tensile.strength - 180 if tensile.strength > 180 or 0 otherwise}
#' }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"rubber"

#' Dataset food
#'
#' From Cleveland (1993): The food web for the animal species in an ecosystem is a description of who eats whom. A chain is a path through the web. It begins with a species that is eaten by no other, moves to a species that the first species eats, moves next to a species that the second species eats, and so forth until the chain ends at a species that preys on no other. If there are 7 species in the chain then there are 6 links between species, and the length of the chain is 6. The mean chain length of a web is the mean of the lengths of all chains in the web. A two-dimensional ecosystem lies in a flat environment such as a lake bottom or a grassland; movement of species in a third dimension is limited. In a three-dimensional ecosystem, there is considerable movement in three dimensions. One example is a forest canopy; another is a water column in an ocean or lake. A mixed ecosystem is made up of a two-dimensional environment and a three-dimensional environment with enough links between the two to regard it as a single ecosystem. An interesting study reports the mean chain lengths for 113 webs.
#'
#' @format A data frame with 113 rows and 2 variables:
#' \describe{
#'   \item{mean.length}{mean web chain length}
#'   \item{dimension}{ecosystem dimenson}
#'   }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"food"

#' Dataset bin
#'
#' From Cleveland (1993): Bin packing is a computer problem that has challenged mathematicians working on the foundations of theoretical computer science. Suppose a large number of files of different sizes are to be written on floppies. No file can be split between two floppies, but we want to waste as little space as possible. Unfortunately, any algorithm that guarantees the minimum possible empty space takes an enormous amount of computation time unless the number of files is quite small. Fortunately, there are heuristic algorithms that run fast and do an extremely good job of packing, even though they do not guarantee the minimum of empty space. One is first fit decreasing. The files are packed from largest to smallest. For each file, the first floppy is tried; if it has sufficient empty space, the file is written, and if not, the second floppy is tried. If the second file has sufficient space, the file is written and if not, the third floppy is tried. The algorithm proceeds in this way until a floppy with space, possibly a completely empty one, is found. To supplement the theory of bin packing with empirical results, mathematicians and computer scientists have run simulations, computer experiments in which bins are packed with randomly generated weights. For one data set from one experiment, the weights were randomly selected from the interval 0 to 0.8 and packed in bins of size one. The number of weights, n, for each simulation run took one of 11 values: 125,250,500, and so forth by factors of 2 up to 128000. There were 25 runs for each of the 11 different numbers of weights, which makes 25 x 11 = 275 runs in all. For each run of the experiment, the performance of the algorithm was measured by the total amount of empty space in the bins that were used. We will study log empty space to enhance our understanding of multiplicative effects.
#'
#' @format A data frame with 275 rows and 2 variables:
#' \describe{
#'   \item{empty.space}{total amount of empty space in the bins that were used}
#'   \item{number.weights}{number of weights}
#'   }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"bin"


#' Dataset ganglion
#'
#' From Cleveland (1993): For species with highly developed visual systems, such as cats and man, the distribution of ganglion cells across the surface of the retina is not uniform. For example, cats at birth have a much greater density of cells in the central portion of the retina than on the periphery. But in the early stages of fetal development, the distribution of ganglion cells is uniform. The nonuniformity develops in later stages. The data presents the measurement for 14 cat fetuses ranging in age from 35 to 62 days of gestation of the ratio of the central ganglion cell density to the peripheral density and their retinal area, which is nearly monotonically increasing with age.
#'
#' @format A data frame with 14 rows and 2 variables:
#' \describe{
#'   \item{area}{retinal area}
#'   \item{cp.ratio}{ratio of the central ganglion cell density to the peripheral density}
#'   }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"ganglion"



#' Dataset dating
#'
#' From Cleveland (1993): Ages of many ancient objects are determined by carbon dating. A second dating method, first reported in 1990, provides calibration back to at least 30 kyr BP by measuring the decay of uranium to thorium. The group that invented the method took core samples in coral off the coast of Barbados and dated the material back to nearly 30 kyr BP using both the carbon and thorium methods. The thorium results were used to study the accuracy of the carbon method.
#'
#' @format A data frame with 19 rows and 2 variables:
#' \describe{
#'   \item{carbon}{carbon age}
#'   \item{thorium}{thorium age}
#'   }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"dating"


#' Dataset polarization
#'
#' From Cleveland (1993): This data comes from an experiment on the scattering of sunhght in the atmosphere. One variable is the Babinet point, the scattering angle at which the polarization of sunhght vanishes. The other one is the atmospheric concentration of soHd particles in the air. The goal is to determine the dependence of the Babinet point on concentration.
#'
#' @format A data frame with 355 rows and 2 variables:
#' \describe{
#'   \item{concentration}{particulate concentration}
#'   \item{babinet}{Babinet point}
#'   }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"polarization"

#' Dataset fly
#'
#' From Cleveland (1993): In 1924, a journal article reported 823 observations from a genetics experiment on flies' eyes. Stocks of the ubiquitous species Drosophila melanogaster Meig were hatched in nine incubators whose temperatures varied from 15°C to 31°C in equal steps of 2°C. The number of facets of the eyes of each hatched fly were reported in units that essentially make the measurement scale logarithmic. The goal of the experiment was to see how facet number depends on temperature.
#'
#' @format A data frame with 823 rows and 2 variables:
#' \describe{
#'   \item{facet}{number of facets of the eyes}
#'   \item{temperature}{incubator temperature}
#'   }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"fly"

#' Dataset playfair
#'
#' From Cleveland (1993): In 1801, William Playfair published his Statistical Breviary, which contains many displays of economic and demographic data. One display, beautifully reproduced by Tufte, graphs the populations of 22 cities by the areas of circles. The graph also contains a table of the populations, so we can compare the data and the areas of the circles.
#'
#' @format A data frame with 22 rows and 2 variables:
#' \describe{
#'   \item{city}{city}
#'   \item{population}{population}
#'   \item{diameter}{diameter of the circle in the figure}
#'   }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"playfair"

#' Dataset etanol
#'
#' From Cleveland (1993): An experiment studied exhaust from an experimental one-cylinder engine fueled by ethanol. The response, which will be denoted by NOx, is the concentration of nitric oxide, NO, plus the concentration of nitrogen dioxide, NO2, normalized by the amount of work of the engine. The units are microg/xg of NOx per joule. One factor is the equivalence ratio, E, at which the engine was run. E is a measure of the richness of the air and fuel mixture; as E increases there is more fuel in the mixture. Another factor is C, the compression ratio to which the engine is set. C is the volume inside the cylinder when the piston is retracted, divided by the volume when the piston is at its maximum point of penetration into the cylinder. There were 88 runs of the experiment.
#' @format A data frame with 88 rows and 2 variables:
#' \describe{
#'   \item{NOx}{concentration of nitric oxide plus the concentration of nitrogen dioxide normalized by the amount of work of the engine.}
#'   \item{C}{compression ratio to which the engine is set}
#'   \item{E}{equivalence ratio at which the engine was run}
#'   }
#' @source Cleveland W. S. (1993). “Visualizing Data”. Hobart Press.
"etanol"


