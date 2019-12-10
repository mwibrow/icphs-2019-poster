source(file.path("R", "settings.R"))

dataDir <- "data"

rad <- function(a) a / 180 * pi

bark <- function(x) {
  return (26.81 * log(1 + x / (x + 1960)) - 0.53)
}
lobanov <- function(df, f1="f1", f2="f2", vowel="vowel", group=c(), reduce=TRUE) {

  ddply(df, group, function(df.grp) {
    f1.grp <- df.grp[,f1]
    f2.grp <- df.grp[,f2]
    mn.f1.grp <- mean(f1.grp, na.rm=TRUE)
    mn.f2.grp <- mean(f2.grp, na.rm=TRUE)
    sd.f1.grp <- sd(f1.grp, na.rm=TRUE)
    sd.f2.grp <- sd(f2.grp, na.rm=TRUE)
    ddply(df.grp, vowel, function(df.vwl) {
      f1.vwl <- df.vwl[,f1]
      f2.vwl <- df.vwl[,f2]
      f1.vwl.nrm <- (f1.vwl - mn.f1.grp) / sd.f1.grp
      f2.vwl.nrm <- (f2.vwl - mn.f2.grp) / sd.f2.grp
      if (reduce) {
        f1.vwl.nrm <- median(f1.vwl.nrm)
        f2.vwl.nrm <- median(f2.vwl.nrm)
      }
      data.frame(f1=f1.vwl.nrm, f2=f2.vwl.nrm)
    })
  })
}


ssbe.df <- read.csv(file.path(dataDir, "ssbe.csv"))
ssbe.df <- subset(ssbe.df, vowel %in% monophthongs)
ssbe.df <- data.frame(
  vowel=c("heed", "hid", "head", "had", "hard", "hod", "hoard", "hood", "whod", "hud", "heard"),
  f1=c(273, 386, 527, 751, 655, 552, 452, 397, 291, 623, 527),
  f2=c(2289, 2038, 1801, 1558, 1044, 986, 793, 1550, 1672, 1370, 1528))

# ssbe.df <- read.csv(file.path(dataDir, "hawkins_midgely_2005.csv"))
ssbe.df <- read.csv(file.path(dataDir, "F1F2-50.csv"))

ssbe.lob.df <- lobanov(ssbe.df, group=c("speaker"))

ssbe.lob.df <- ddply(ssbe.lob.df, c("vowel"), function(subset) {
  data.frame(f1=mean(subset$f1), f2=mean(subset$f2))
})
ssbe.lob.df$label <- hvd[as.character(ssbe.lob.df$vowel)]

ssbe.lob.df <- rbind(
    ddply(ssbe.lob.df, names(ssbe.lob.df), function(x) data.frame(group="LV")),
    ddply(ssbe.lob.df, names(ssbe.lob.df), function(x) data.frame(group="HV")))

ssbe.lob.df$angle <- 0
ssbe.lob.df$dist <- 0.25

lob.file <- file.path(dataDir, "lobanov.csv")
if (file.exists(lob.file)) {
  cat("Calculating Lobanov formats.\n")
  vwl.df <- rbind(
    read.csv(file.path(dataDir, "pre-LV.csv")),
    read.csv(file.path(dataDir, "post-LV.csv")),
    read.csv(file.path(dataDir, "pre-HV.csv")),
    read.csv(file.path(dataDir, "post-HV.csv")))
  cat(sprintf('Processing %d rows\n', nrow(vwl.df)))
  # vwl.df <- vwl.df[vwl.df$vowel %in% monophthongs,]
  vwl.df$speaker <- sapply(vwl.df$participant, FUN=function(x) sub("(C[0-9]+).*?([LH]V)", "\\1\\2", x))



  lob.df <- lobanov(vwl.df, f1="f1", f2="f2", group=c("group", "test", "speaker"))
  lob.df <- subset(lob.df, vowel %in% monophthongs)
  write.csv(lob.df, file.path(dataDir, "lobanov-calc.csv"), row.names=FALSE, quote=FALSE)
} else {
  cat("Reading precalculated Lobanov formats\n")
  lob.df <- read.csv(lob.file)
  lob.df$test <- as.factor(lob.df$test)
  lob.df$test <- relevel(lob.df$test, ref="pre")
}

lob.mn.df <- ddply(lob.df, c("group", "test", "vowel"), function(subset) {
  data.frame(f1=mean(subset$f1), f2=mean(subset$f2))
})


# Do plot


df.melt <- melt(lob.mn.df,  id.var = c("vowel", "group", "test"))
df <- dcast(df.melt, group+vowel~variable+test)
df$hvd <- hvd[as.character(df$vowel)]


lab.df <- with(df, data.frame(label=hvd, group, f1=f1_pre, f2=f2_pre))




label.transform <- function(
  df, x, y,
  label="label",
  transforms=NULL,
  groups=c(),
  default=list(angle=0, r=0.25)) {
  return(ddply(df, c(groups, label), function(d) {
    group <- as.character(d$group)
    label <- as.character(d$label)
    if (is.null(transforms)) {
      pos = NULL
    } else {
      pos <- transforms[[group]][[label]]
    }
    if (is.null(pos)) {
      pos <- default
    } else {
      if (is.numeric(pos)) {
        pos <- list(angle=pos, r=default$r)
      }
    }
    data.frame(
        x=d[,x] - cos(rad(pos$angle)) * pos$r,
        y=d[,y] - sin(rad(pos$angle)) * pos$r)
  }))
}

dict <- function(content=NULL) {
  dct <- new.env(hash=TRUE)
  if (!is.null(content)) {
    expressions <- parse(text = deparse(substitute(content)))[[1]]
    for (i in seq(2, length(expressions))) {
      expression <- expressions[[i]]
      name <- as.character(expression[[2]])
      value <- expression[[3]]
      dct[[name]] <- eval(value)
    }
  }
  return (dct)
}

transforms <- new.env(hash=TRUE)
with(transforms, {
  LV <- new.env(hash=TRUE)
  HV <- new.env(hash=TRUE)
  with(LV, {
    "ɒ" <- 180
    "i:" <- 0
    "ʊ" <- 180
    "ɪ" <- 180
    "ɜ:" <- 180
    "ʌ" <- list(angle=215, r=.333)
    "æ" <- 210
    "e" <- 180
    "ɔ:" <- 90
    "ɑ:" <- 300
  })
  with(HV, {
    "i:" <- 90
    "æ" <- 180
    "ɪ" <- 0
    "ɜ:" <- list(angle=90, r=0.55)
    "e" <- 90
    "ɑ:" <- list(angle=90,r=0.2)
    "ʌ" <- list(angle=255, r=0.4)
    "u:" <- 90
    "ɒ" <- 0
      "ɔ:" <- 0
      "ʊ" <- 270
  })
})

transforms.ssbe <- new.env(hash=TRUE)
with(transforms.ssbe, {
  LV <- new.env(hash=TRUE)
  HV <- new.env(hash=TRUE)
  with(LV, {
    "ʌ" <- 270
    "e" <- 270
    "ɪ" <- 270
    "ɑ:" <- 0
    "ɒ" <- 0
       "u:" <- 0
  })
  with(HV, {
    "æ" <- 180
    "ɪ" <- 270
    "e" <- 180
    "ʌ" <- 270
     "u:" <- 0
     "ɒ" <- 0
      "ɑ:" <- 0
  })
})

lab.df <- label.transform(
  lab.df, x="f2", y="f1",
  groups=c("group"),
  transforms=transforms,
  default=list(angle=0, r=0.22))

ssbe.lab.df <- ssbe.lob.df[c("label", "group", "f1", "f2")]
ssbe.lab.df <- label.transform(
  ssbe.lab.df, x="f2", y="f1",
  groups=c("group"),
  transforms=transforms.ssbe,
  default=list(angle=90, r=0.22))


DPI = 600
showtext_opts(dpi=DPI)
fontSize <- 12
width <- 3.333
height = 7.25

colors$ssbe = '#999999'
# Start plot and set some theme stuff
p <- ggplot(data=) + theme(
  text=element_text(family="DejaVuSans", size=fontSize),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "#dddddd", fill=NA, size=1),
  #plot.background=element_rect(fill="white", color="transparent"),
  #panel.background=element_rect(fill=colors$panel.background, color="transparent"),
  panel.grid.major=element_line(
    color="#dddddd",
    linetype="13",
    lineend="round"),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  axis.ticks=element_blank(),
  legend.position="bottom",
  # legend.justification=c(0.5,0),
  legend.key=element_rect(
    fill="white",
    colour="white"),
  legend.background=element_rect(
    fill="white",
    colour="white"
  ),
  legend.spacing.y=unit(.1, "cm"),
  legend.title=element_blank(),
  axis.title.x = element_text(
    margin = margin(t = 7.5, r = 0, b = 0, l = 0),
    size=fontSize*0.875),
  axis.title.y = element_text(
    size=fontSize*0.875),
  legend.margin=margin(t=0,r=8,b=4,l=2),
  legend.box.margin=margin(-2,0,0,0),
  # strip.background = element_blank(),
  # strip.text.x = element_blank(),
  # strip.text.y = element_blank()
  )
# Reverse scales
p <- p + scale_y_reverse(
  expand=c(0.02,0.02),
  minor_breaks=c(),
  breaks=seq(-3,3,1),
  labels=seq(-3,3,1))
p <- p + scale_x_reverse(
  expand=c(0.02,0.02),
  minor_breaks=c(),
  breaks=seq(-3,3,1),
  labels=seq(-3,3,1))
# Add limits here to prevent filtering of data
p <- p + coord_cartesian(xlim=c(2.1,-2.1), ylim=c(2.1, -2.1))

# Add SSBE points...
ssbe.lob.df$ssbe = 'ssbe'
p <- p + geom_point(
  data=ssbe.lob.df,
  aes(x=f2, y=f1, shape=ssbe, color=ssbe),
  size=2)
# ...and SSBE labels
p <- p + geom_text(
  data=ssbe.lab.df,
  aes(x, y, label=label),
  color=colors$ssbe.label,
  family="DejaVuSans",
  vjust=0.4,
  size=fontSize*0.4)


# Draw lines between pre and post
p <- p + geom_segment(
  data=df,
  aes(x=f2_pre, xend=f2_post, y=f1_pre, yend=f1_post),
  # arrow=arrow(type="closed", length=unit(0.075, "in")),
  inherit.aes=FALSE,
  colour=colors$arrow)

# Draw pre and post points
p <- p + geom_point(
  data=lob.mn.df,
  aes(x=f2, y=f1, shape=test, color=test),
  size=2)
# p <- p + scale_discrete_manual(
#   c("shape"),
#   values=c(20,19,18),
#   labels=c('Pre-test', 'Post-test', 'SSBE')
# )
p <- p + scale_color_manual(
  breaks=c('pre', 'post', 'ssbe'),
  labels=c('Pre-test', 'Post-test', 'SSBE'),
  values=c(colors$pre, colors$post, colors$ssbe),
  name="Test")
p <- p + scale_shape_manual(
  breaks=c('pre', 'post', 'ssbe'),
  labels=c('Pre-test', 'Post-test', 'SSBE'),
  values=c(19,20,18),
  name="Test")


# Add labels to pre points
p <- p + geom_text(
  data=lab.df,
  aes(x, y, label=label),
  colour=colors$ipa,
  family="DejaVuSans",
  vjust=0.4,
  size=fontSize*0.4)

# groupDf <- data.frame(f1=1.75, f2=-1.625, group=c("LV", "HV"))
# p <- p + geom_label(
#   data=groupDf,
#   aes(x=f2, y=f1, label=group),
#   family="DejaVuSans", label.size=0, fill="#cccccc")

p <- p +  ylab("F1 (Lobanov)")
p <- p +  xlab("F2 (Lobanov)")
p <- p + facet_wrap(vars(group), nrow=2,ncol=1,strip.position="top")
# p <- p + ggtitle('Acoustic analysis',
#   subtitle="Comparison of monopththong productions with SSBE prototypes")
options(repr.plot.width=width, repr.plot.height=height)
suppressGraphics(
  ggsave(file.path(outDir, "vowel-plot.jpg"), device="jpeg", width=width, height=height, units="in", dpi=DPI))
