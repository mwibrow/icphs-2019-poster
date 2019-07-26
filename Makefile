latexmk=latexmk
latex=lualatex
biber=biber
output_dir=build
input_dir=src
syntex=1
gs=gs
target=main
jobname=poster
src=$(input_dir)/$(target).tex
plots=$(input_dir)/plots
screenshots=$(input_dir)/images/CALVin-screenshots
TEXINPUTS:=$(input_dir)//:$(TEXINPUTS)
BIBINPUTS:=$(input_dir)//:$(BIBINPUTS)
dpi=300
title=icphs-2019-poster.pdf
underline=printf '=%.0s' {1..40}


all: images plots poster

final: dpi=900
final: title=icphs-2019-poster.pdf
final: all

screen: dpi=300
screen: title=icphs-2019-poster-screen.pdf
screen: all

poster: paths latex biber rerun rererun
	cp $(output_dir)/$(jobname).pdf ./$(title)

latex rerun rererun: $(src)
	TEXINPUTS="$(TEXINPUTS)" $(latex) -recorder --output-directory=$(output_dir) --interaction=nonstopmode --jobname=$(jobname) $(src)

biber:
	BIBINPUTS="$(BIBINPUTS)" $(biber) $(output_dir)/$(jobname)

paths:
	@ echo; echo Creating paths; echo `$(underline)`; \
	mkdir -p build src/plots/images $(screenshots)/jpgs

plots: paths
	@ cd $(plots); \
	echo; echo Creating plots; echo `$(underline)`; \
	export DPI=$(dpi); \
	for script in *.R; do echo Executing $$script; Rscript $$script; done

images: paths
ifeq ($(dpi), 300)
	@ cd $(screenshots); \
	echo; echo Scaling and converting images $(dpi); echo `$(underline)`; \
	mogrify -verbose -format jpg -path jpgs -scale 10% pngs/*.png;
else
    @ cd $(screenshots); \
	echo; echo Converting images; echo `$(underline)`; \
	mogrify -verbose -format jpg -path jpgs pngs/*.png
endif


clear-fontcache:
	luaotfload-tool --cache=erase

clean: FORCE
	rm -R build; \
	rm *.pdf

FORCE:
