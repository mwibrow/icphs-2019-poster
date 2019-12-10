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

width <- 3.3333
height <- 3.5


colors$pre = '#999999'

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
    breaks=c("pre", "post"),
    labels=c("Pre", "Post"),
    values=c(pre=colors$pre, post=colors$post),
    name="Test"
)
p <- p + scale_color_manual(
    breaks=c("pre", "post"),
    labels=c("Pre", "Post"),
    values=c(pre=colors$pre, post=colors$post),
    name="Test")

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

p <- p + theme(
   plot.title = element_text(hjust = 0.5),
	# Set the default font
	 panel.background = element_blank(),
  panel.border = element_rect(colour = "#dddddd", fill=NA, size=1),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
  #panel.background=element_rect(fill="transparent"),#colors$panel.background),
  panel.grid.major=element_line(
    color="#dddddd",#colors$panel.grid,
    linetype="13",
    lineend="round"),
   text=element_text(family="DejaVuSans", size=fontSize),
  axis.ticks=element_blank(),
   axis.text.x = element_text(size=fontSize*0.75),
    axis.text.y = element_text(size=fontSize*0.75),
legend.position="bottom",
legend.margin=margin(t=0,r=0,b=0,l=0),
  legend.key=element_rect(
    fill="transparent",
    colour="transparent"))



suppressGraphics(ggsave(
  file.path(
    outDir,
    "Figure5.jpg"),
  device="jpg",
  width=width,
  height=height,
  units="in",
  dpi=DPI))
