
cutter <- \(x, border) {  crop(x, floor(ext(border))) |> mask(mask=border)}


get_crops <- function(crops, border, path=".", africa=TRUE, var="area", type="A") {
	# crop area 
	type <- match.arg(toupper(type), c("A", "I", "R"))
	i <- match(type, c("A", "I", "R"))
	var = match.arg(tolower(var), c("area", "density", "proportion"))
	crps <- lapply(crops, \(name) geodata::crop_spam(name, var="area", path=path, africa=africa)[[i]]) |> rast() |> cutter(border)
	#crps <- ifel(crps==0, NA, crps)
	names(crps) <- crops
	if (var == "density") {
		# 100 * 10000 m2 / m2 = %
		crpsdens <- 1000000 * crps / cellSize(crps)
	} else if (var == "proportion") {
		crps / (sum(values(crps, na.rm=TRUE)) / 100)
	} else {
		crps
	}
}


