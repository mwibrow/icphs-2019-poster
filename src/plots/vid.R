source(file.path("R", "settings.R"))

# Ensure packages are installed
list.of.packages <- c("ggplot2", "showtext", "plyr", "reshape2", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (package in list.of.packages) {
  library(package, character.only=TRUE)
}

font_add("Cabin", "../fonts/Cabin/Cabin-Regular.ttf")
font_add("Cabin-Italic", "../fonts/Cabin/Cabin-Italic.ttf")
font_add("DejaVuSans", "../fonts/dejavu-fonts-ttf-2.37/ttf/DejaVuSans.ttf")

showtext_auto()

dataDir <- "data"

vid.df = read.csv(file.path(dataDir, "VID.csv"))

bx.df = ddply(vid.df, c("Group", "Test", "Participant"), function(subset) {
    nTrials = sum(subset$nTrials)
    nCorrect = sum(subset$nCorrect)
    Accuracy = nCorrect / nTrials * 100
    data.frame("Group"=subset$Group, "Test"=subset$Test, "Accuracy"=Accuracy)
})

bx.df$Group = factor(bx.df$Group, levels=c("LV", "HV"))
bx.df$Test = factor(bx.df$Test, levels=c("pre", "post"))

POST <- "#F8BBD0"
PRE <- "#E91E63"
MD <- "#FCE4EC"
IPA <- "#176FC1"


dodge <- position_dodge(0.875)

width <- 5
height <- 3

DPI <- 600

dpi <- DPI

showtext_opts(dpi=dpi)
fontSize <- 12

p <- ggplot(data=bx.df)
p <- ggplot(
  bx.df,
  aes(
    x=Group,
    y=Accuracy,
    fill=Test,
    color=Test))
p <- p + geom_boxplot(
    position=dodge,
    aes(fill=Test, color=Test))

p <- p + scale_y_continuous(
  expand=c(0, 5),
  limit=c(0, 100),
	minor_breaks=c(),
	breaks=seq(0, 100, 25),
	labels=seq(0, 100, 25))

p <- p + scale_fill_manual(
  values=c(pre=PRE, post=POST))
p <- p + scale_color_manual(
  values=c(pre=PRE, post=POST))

dat <- ggplot_build(p)$data[[1]]
dat <- cbind(
	with(
    bx.df,
    expand.grid(
      Test=levels(Test),
      Group=levels(Group))),
	dat)
p <- p + geom_segment(
  data=dat,
  aes(
    x=xmin,
    xend=xmax,
    y=middle,
    yend=middle),
  lineend="square",
  inherit.aes=FALSE,
  colour=MD)

p <- p + theme(
	# Set the default font
	text=element_text(
    family="Cabin",
    size=fontSize),
	# Set the legend position
	legend.position="right",
	# Remove the panel background and grid from the legend
	legend.key=element_rect(
	  fill="transparent",
	  colour="transparent"),
	# Change the color of the graph background
	panel.background=element_rect(
    fill="#eeeeee"),
	# Make the grid lines dashed
	panel.grid.major=element_line(
    color="#ffffff",
    linetype="13",
    lineend="round"),
	# Remove tick marks
	axis.ticks = element_blank(),
	axis.title.x = element_text(
    margin=margin(t=5, r=0, b=0, l=0)))


ggsave(
  file.path(
    outDir,
    "vid-boxplot.png"),
  width=width,
  height=height,
  units="in",
  dpi=dpi)
