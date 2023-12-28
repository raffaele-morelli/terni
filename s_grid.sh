#!/bin/sh

# traccianti ####
Rscript f_grid.R Cr_i &
Rscript f_grid.R Mo_s &
Rscript f_grid.R Ni_i &
wait

Rscript f_grid.R Cs_s &
Rscript f_grid.R Rb_i &
Rscript f_grid.R U_i &
wait

Rscript f_grid.R Cu_i &
Rscript f_grid.R Sb_i &
Rscript f_grid.R Sn_i &
wait

Rscript f_grid.R Cs_s &
Rscript f_grid.R K_s &
Rscript f_grid.R Rb_s &
wait

Rscript f_grid.R Tl_s &
Rscript f_grid.R Rb_s &
wait