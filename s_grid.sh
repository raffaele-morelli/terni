#!/bin/sh
cd funzioni;

Rscript f_grid.R Al_i $1 &
Rscript f_grid.R Bi_i $1 &
Rscript f_grid.R Ca_s $1 &
Rscript f_grid.R Cd_s $1 &
Rscript f_grid.R Cr_i $1 &
Rscript f_grid.R Cr_s $1 &
Rscript f_grid.R Cs_s $1 &
Rscript f_grid.R Cu_i $1 &
Rscript f_grid.R Cu_s $1 &
wait 

Rscript f_grid.R Fe_i $1 &
Rscript f_grid.R K_s $1 &
Rscript f_grid.R Li_i $1 &
Rscript f_grid.R Li_s $1 &
Rscript f_grid.R Mg_i $1 &
Rscript f_grid.R Mg_s $1 &
Rscript f_grid.R Mn_i $1 &
Rscript f_grid.R Mn_s $1 &
Rscript f_grid.R Mo_i $1 &
wait

Rscript f_grid.R Mo_s $1 &
Rscript f_grid.R Na_i $1 &
Rscript f_grid.R Na_s $1 &
Rscript f_grid.R Ni_i $1 &
Rscript f_grid.R Pb_i $1 &
Rscript f_grid.R Pb_s $1 &
Rscript f_grid.R PM10 $1 &
Rscript f_grid.R Rb_s $1 &
Rscript f_grid.R Sb_s $1 &
wait 

Rscript f_grid.R Sn_i $1 &
Rscript f_grid.R Sn_s $1 &
Rscript f_grid.R Sr_s $1 &
Rscript f_grid.R Ti_i $1 &
Rscript f_grid.R Tl_s $1 &
Rscript f_grid.R V_s $1 &
Rscript f_grid.R W_s $1 &
Rscript f_grid.R Zn_s $1 &
Rscript f_grid.R Zr_i $1 &

