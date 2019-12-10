source(file.path("R", "settings.R"))


pit.df = read.csv(file.path(dataDir, "PIT.csv"))
pint.df = read.csv(file.path(dataDir, "pint.csv"))

pit.df$Task = 'Picture identification'
pint.df$Task = 'Picture naming'

bx1.df = ddply(pit.df, c("Task", "group", "participant"), function(subset) {
    nTrials = nrow(subset)
    nCorrect = sum(subset$correct)
    Accuracy = nCorrect / nTrials * 100
    data.frame(
        "Task"=subset$Task[1],
        "Group"=subset$group[1], "Participant"=subset$participant[1], "Accuracy"=Accuracy)
})

bx2.df = ddply(pint.df, c("Task", "group", "participant"), function(subset) {
    nTrials = nrow(subset)
    nCorrect = sum(subset$correct)
    Accuracy = nCorrect / nTrials * 100
    data.frame(
        "Task"=subset$Task[1],
        "Group"=subset$group[1], "Participant"=subset$participant[1], "Accuracy"=Accuracy)
})

bx.df <- rbind(bx1.df, bx2.df)

bx.df$Group = factor(bx.df$Group, levels=c("LV", "HV"))
bx.df$Task = factor(bx.df$Task, levels=c("Picture identification", "Picture naming"))
POST <- "#F8BBD0"
PRE <- "#E91E63"
MD <- "#FCE4EC"
IPA <- "#176FC1"

colors$pre = '#777777'

dodge <- position_dodge(0.875)

width <- 3.33
height <- 3.5



showtext_opts(dpi=DPI)
fontSize <- 10

colors$pre = '#999999'

p <- ggplot(data=bx.df)
p <- ggplot(
  bx.df,
  aes(
    x=Group,
    y=Accuracy,
    color=Group,
    fill=Group))
p <- p + geom_boxplot(
    position=dodge)

p <- p + scale_y_continuous(
  expand=c(0, 5),
  limit=c(0, 100),
	minor_breaks=c(),
	breaks=seq(0, 100, 25),
	labels=seq(0, 100, 25))


p <- p + scale_fill_manual(
    breaks=c("LV", "HV"),
    labels=c("LV", "HV"),
    values=c(LV=colors$post, HV=colors$post),
    name="Group"
)
p <- p + scale_color_manual(
    breaks=c("LV", "HV"),
    labels=c("LV", "HV"),
    values=c(LV=colors$post, HV=colors$post),
    name="Group"
)
p <- p + facet_wrap(vars(Task), nrow=1,ncol=2,strip.position="top")

dat <- ggplot_build(p)$data[[1]]

dat <- cbind(
	with(
    bx.df,
    expand.grid(
    Group=levels(Group),
      Task=levels(Task)
      )),
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
   axis.title.y = element_text(
    margin=margin(t=0, r=0, b=0, l=0)),
  axis.ticks=element_blank(),
   axis.text.x = element_text(size=fontSize*0.75),
    axis.text.y = element_text(size=fontSize*0.75),
legend.position="none",
legend.margin=margin(t=0,r=0,b=0,l=0),
  legend.key=element_rect(
    fill="transparent",
    colour="transparent"))

#p <- p + ggtitle('Vowel intelligibility')

suppressGraphics(ggsave(
  file.path(
    outDir,
    "Figure10.jpg"),
  device="jpeg",
  width=width,
  height=height,
  units="in",
  dpi=DPI));
