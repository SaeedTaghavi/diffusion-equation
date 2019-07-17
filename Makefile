#by Saeed Taghavi
COMPILER = gfortran
FLAGS = -O0 
EXEFILE = run.exe
BASE = $(shell pwd)

default : commands

## commands   : show all commands.
commands :
	@grep -E '^##' Makefile | sed -e 's/## //g'

## clean      : clean up all the previous outputs, and executable.
clean: cleanPlots
	@echo "Cleaning ......"
	@rm -f $(BASE)/*.txt
	@rm -f $(BASE)/$(EXEFILE)
	@rm -f $(BASE)/output/*.txt

## cleanPlots : clean up all the previous plots.
cleanPlots:
#	@rm -f $(BASE)/plots/*.svg
#	@rm -f $(BASE)/plots/*.png
	@rm -f $(BASE)/plots/*.eps

## plot       : ploting outputs.	
plot:
	@gnuplot $(BASE)/plots/plot-map.gpt
	@mkdir -p $(BASE)/plots/map
	@mv $(BASE)/output/*.png $(BASE)/plots/map

	@gnuplot $(BASE)/plots/plot3d.gpt
	@mkdir -p $(BASE)/plots/3d
	@mv $(BASE)/output/*.png $(BASE)/plots/3d
#todo create svg and eps plots
#	@mv $(BASE)/*.svg $(BASE)/plots
#	@mv $(BASE)/*.eps $(BASE)/plots
#	@mv $(BASE)/*.png $(BASE)/plots

## animation  : make animation out of the results.
animation:
	@gnuplot $(BASE)/plots/animation-3d.gpt
	@mkdir -p $(BASE)/plots/animation
	@mv $(BASE)/*.gif $(BASE)/plots/animation/3d-animation.gif

	@gnuplot $(BASE)/plots/animation-map.gpt
	@mkdir -p $(BASE)/plots/animation
	@mv $(BASE)/*.gif $(BASE)/plots/animation/map-animation.gif

## compile    : compiling the source file and creating the executable file.
compile:
	@echo  "Compiling ...."
	@echo
	$(COMPILER) $(BASE)/src/*.f90 -o $(EXEFILE)
	@mv $(EXEFILE) $(BASE)/bin

## run        : running the executable.
run: 
	@echo "running ...."
	$(BASE)/bin/$(EXEFILE)
	@mv $(BASE)/*.txt $(BASE)/output
	@mv $(BASE)/*.dat $(BASE)/output
	@rm -rf $(BASE)/cg.log