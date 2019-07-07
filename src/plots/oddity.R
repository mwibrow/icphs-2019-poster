source(file.path("R", "settings.R"))


odd.df = read.csv(file.path(dataDir, "oddity.csv"))

bx.df = ddply(odd.df, c("Group", "Test", "participant"), function(subset) {
    nTrials = sum(subset$nTrials)
    nCorrect = sum(subset$nCorrect)
    Accuracy = nCorrect / nTrials * 100
    data.frame("Group"=subset$Group, "Test"=subset$Test, "Accuracy"=Accuracy)
})

bx.df$Group = factor(bx.df$Group, levels=c("LV", "HV"))
bx.df$Test = factor(bx.df$Test, levels=c("pre", "post"))



dodge <- position_dodge(0.875)

width <- 3.75
height <- 3.5



showtext_opts(dpi=DPI)
fontSize <- 14

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
  values=c(pre=colors$pre, post=colors$post))
p <- p + scale_color_manual(
  values=c(pre=colors$pre, post=colors$post))

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
  colour=colors$median)

p <- p + ggtitle('Category discrimination')
p <- p + theme(
   plot.title = element_text(hjust = 0.5),
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
    fill=colors$panel.background),
	# Make the grid lines dashed
	panel.grid.major=element_line(
    color=colors$panel.grid.major,
    linetype="13",
    lineend="round"),
	# Remove tick marks
	axis.ticks = element_blank(),
	axis.title.x = element_text(
    margin=margin(t=5, r=0, b=0, l=0)))



suppressGraphics(ggsave(
  file.path(
    outDir,
    "oddity-boxplot.png"),
  width=width,
  height=height,
  units="in",
  dpi=DPI))
