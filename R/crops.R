
cutter <- \(x, border) {  crop(x, floor(ext(border))) |> mask(mask=border)}


get_crops <- function(crops, border, path=".", africa=TRUE, var="area") {
	# crop area 
	var = match.arg(tolower(var), c("area", "density", "proportion"))
	crps <- lapply(crops, \(name) geodata::crop_spam(name, var="area", path=path, africa=africa)[[1]]) |> rast() |> cutter(border)
	#crps <- ifel(crps==0, NA, crps)
	names(crps) <- crops
	if (var == "density") {
		# %
		crpsdens <- 100000 * crps / cellSize(crps)
	} else if (var == "proportion") {
		crps / (sum(values(crps, na.rm=TRUE)) / 100)
	} else {
		crps
	}
}


