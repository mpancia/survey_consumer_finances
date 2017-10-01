all: data plots

data: ../data/scf\ 2016.rds ../data/scf\ 2016\ rw.rds 

../data/scf\ 2016.rds:
	R -f ./R/get_data.R

../data/scf\ 2016\ rw.rds:
	R -f ./R/get_data.R

plots: ./plots/racial_plot.png ./plots/racial_plot_transparent.png ./plots/racial_plot_linetypes.png ./plots/racial_plot_linetypes_transparent.png data 

./plots/racial_plot.png ./plots/racial_plot_transparent.png ./plots/racial_plot_linetypes.png ./plots/racial_plot_linetypes_transparent.png: plots.intermediate
.INTERMEDIATE: plots.intermediate

plots.intermediate:
	R -f ./R/make_plots.R
