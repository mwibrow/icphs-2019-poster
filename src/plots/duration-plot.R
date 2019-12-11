
source(file.path("R", "settings.R"))

dataDir <- "data"

durDf = read.csv(file.path(dataDir, "duration-ssbe.csv"))
durDf$Group = factor(durDf$Group, c("LV", "HV", "SSBE"))
durDf$Test = factor(durDf$Test, c("pre", "post", "SSBE"))

palette <- new.env()
with(palette, {
  default <- "black"
  median <- "white"
  pre <- "gray50"
  post <- "gray0"
  ssbe <- "gray100"
  border <- "grey70"
  grid <- "gray70"
})

palette.color <- function(name) {
  if (nchar(name) == 0) {
    return (palette$default)
  }
  if (is.null(palette[[name]])) {
    return (palette.color(sub("\\.*[^.]*$", "", name)))
  }
  return (palette[[name]])
}

color <- palette.color

dpi <- 600
width <- 3.333
height <- 3.5
options(repr.plot.width=width, repr.plot.height=height)

showtext_opts(dpi=dpi)
fontSize <- 10
fontFamily <- "DejaVuSans"

theme_update(
  panel.background = element_blank(),
  panel.border = element_rect(colour = color("border"), fill=NA, size=1),
  panel.grid.major = element_line(
    color = color("grid"),
    linetype = "13",
    lineend = "round"
  ),
  text = element_text(
    family = "DejaVuSans",
    size = fontSize
  ),
  # Axis stuff
  axis.title.y = element_text(
    margin = margin(t=0, r=1, b=0, l=0)
  ),
  axis.ticks = element_blank(),
  axis.text.x = element_text(size = fontSize*0.75),
  axis.title.x = element_text(hjust = 0.333),
  axis.text.y = element_text(size = fontSize*0.75),
  # Legend stuff
  legend.position = "bottom",
  legend.margin = margin(t = 0, r = 4, b = 0, l = 2),
  legend.key=element_rect(
    fill="transparent",
    colour="transparent"
  )
)

p <- ggplot()

p <- p + geom_boxplot(
  data=durDf, aes(x=Group, y=Duration, color=Test, fill=Test),
    varwidth = TRUE,
    outlier.shape=20)

# This is a workaround to have the boxplot lines color and fill color
# the same without generate extra legend entries.
p <- p + scale_fill_manual(
    breaks=c("pre", "post"),
    labels=c("Pre", "Post"),
    values=c(pre=color("pre.fill"), post=color("post.fill"), SSBE=palette$ssbe),
    name="Test"
)
p <- p + scale_color_manual(
    breaks=c("pre", "post"),
    labels=c("Pre", "Post"),
    values=c(pre=color("pre.color"), post=color("post.color"), SSBE="#000000"),
    name="Test")

p <- p + xlab("Group") +
    ylab("Duration (ms)")

p <- p + scale_y_continuous(
  expand=c(0,0),
  minor_breaks=c(),
  breaks=seq(0,600,150),
  labels=seq(0,600,150))

# Add limits here to prevent filtering of data
p <- p + coord_cartesian(ylim=c(0, 600))

# Medians are usually the same color as the lines of the boxplot
# So to make them a different color have to do this...
medDf <- ggplot_build(p)$data[[1]]
medDf$Test <- c("pre", "post", "pre", "post", "SSBE")
medDf$Group <- c("LV", "LV", "HV", "HV", "SSBE")
medDf$Group <- factor(medDf$Group, levels=c("LV", "HV", "SSBE"))

# Don't need SSBE as the median color is black for SSBE
medDf <- subset(medDf, medDf$Group != "SSBE")
p <- p + geom_segment(
  data = medDf,
  aes(
    x = xmin,
    xend = xmax,
    y = middle,
    yend = middle
  ),
  lineend = "square",
  inherit.aes = FALSE,
  colour = color("median"))


ggsave(file.path(outDir, "Figure7.jpg"),device="jpeg",width=width, height=height, units="in", dpi=dpi)
