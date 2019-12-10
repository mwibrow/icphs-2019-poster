
source(file.path("R", "settings.R"))


dataDir <- "data"

# vwl.df <- rbind(
#     read.csv(file.path(dataDir, "pre-LV.csv")),
#     read.csv(file.path(dataDir, "post-LV.csv")),
#     read.csv(file.path(dataDir, "pre-HV.csv")),
#     read.csv(file.path(dataDir, "post-HV.csv")))
vwl.df = read.csv(file.path(dataDir, "duration-ssbe.csv"))
vwl.df$Group = factor(vwl.df$Group, c('LV', 'HV', 'SSBE'))
vwl.df$Test = factor(vwl.df$Test, c('pre', 'post', 'SSBE'))


#pre.rows <- nrow(vwl.df)
# vwl.df <- ddply(vwl.df, c("group", "test", "vowel"), function(d){

#   duration.mad <- median(abs(d$duration - median(d$duration)))
#   duration.dev <- abs(d$duration - median(d$duration)) / duration.mad
#   cbind(d,
#     data.frame(
#       out=duration.dev> 3
#     )
#   )
# })
# vwl.df <- subset(vwl.df)#, !out)
# post.rows <- nrow(vwl.df)
# cat(sprintf("Removed %d outliers\n", pre.rows - post.rows))

colors$pre = '#999999'
colors$ssbe = '#bbbbbb'
dpi <- 600
width <- 3.333
height <- 3.5
options(repr.plot.width=width, repr.plot.height=height)

showtext_opts(dpi=dpi)
fontSize <- 10

p <- ggplot() + theme(
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
   axis.title.x = element_text(hjust=0.333),
    axis.text.y = element_text(size=fontSize*0.75),
legend.position="bottom",
legend.margin=margin(t=0,r=4,b=0,l=2),
  legend.key=element_rect(
    fill="transparent",
    colour="transparent"))

p <- p + geom_boxplot(
  data=vwl.df, aes(x=Group, y=Duration, color=Test, fill=Test),
    varwidth = TRUE,
    outlier.shape=20)
p <- p + scale_fill_manual(
    breaks=c("pre", "post"),
    labels=c("Pre", "Post"),
    values=c(pre=colors$pre, post=colors$post, SSBE='#ffffff'),
    name="Test"
)
p <- p + scale_color_manual(
    breaks=c("pre", "post"),
    labels=c("Pre", "Post"),
    values=c(pre=colors$pre, post=colors$post, SSBE=colors$post),
    name="Test")

p <- p + xlab("Group") +
    ylab("Duration (ms)")

p <- p + scale_y_continuous(
  expand=c(0,0),
  minor_breaks=c(),
  breaks=seq(0,600,150),
  labels=seq(0,600,150))

# Add limits here to prevent filtering of data
p <- p + coord_cartesian(ylim=c(0,600))

dat <- ggplot_build(p)$data[[1]]
dat$Test <- c("pre", "post", "pre", "post", "SSBE")
dat$Group <- c("LV", "LV", "HV", "HV", "SSBE")

dat$Group <- factor(dat$Group, levels=c("LV", "HV", 'SSBE'))
dat <- head(dat, 4)
p <- p + geom_segment(
  data=dat,
  aes(x=xmin, xend=xmax, y=middle, yend=middle), lineend="square", inherit.aes=FALSE, colour="white")

ggsave(file.path(outDir, "Figure7.jpg"),device="jpeg",width=width, height=height, units="in", dpi=dpi)
