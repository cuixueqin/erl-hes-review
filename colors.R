# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette without black:
cbb_no_black <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# RCP-color
rcp.fill <- c( "RCP2.6" = "#009E73", "RCP4.5" = "#0072B2", "RCP6.0" = "#CC79A7", "RCP8.5" = "#D55E00", "Historic" = "black",
               "Climate and GHGs only" = "#D55E00", "High pollution" = "#D55E00", "A2" = "#D55E00",
               "2C" = "#009E73", "Paris Forever" = "#CC79A7", "Baseline" = "#D55E00")
rcpColorScale <- scale_colour_manual(name = "id", values = rcp.fill )


# Classification colors
class.fill <- c( "Commentary" = "#D55E00", "Coupling example" = "#F0E442", "Exclude" = "#000000", 
                 "Integrated Model" = "#0072B2", "Linking tool" = "#009E73", "Review" = "#CC79A7", "Other" = "#CC79A7")
classFillScale <- scale_fill_manual(name = "Classification", values = class.fill )

# Spataial colors
spatial.fill <- c( "Global" = "#000000", "Regional-China" = "#F0E442", 
                 "Regional-Eurasia" = "#0072B2", "Regional-USA" = "#CC79A7")
spatialFillScale <- scale_fill_manual(name = "Spatial", values = spatial.fill )

# Study shapes
study.shape <- c("Thornton et al. (2017)" = 15, "Yang et al. (2015)" = 16, 
                 "Beckage et al. (2018)" = 17, "Monier et al. (2018)" = 18,
                 "Reilly et al. (2007)" = 5, "Voldoire et al. (2007)" = 6)
studyShapeScale <- scale_shape_manual(name = "Study", values=study.shape)