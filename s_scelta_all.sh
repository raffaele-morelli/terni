#!/bin/sh
cd funzioni

# traccianti ####
Rscript f_scelta_modello.R Al_i $1 &
Rscript f_scelta_modello.R Al_s $1 &
Rscript f_scelta_modello.R As_i $1 &
Rscript f_scelta_modello.R As_s $1 &
Rscript f_scelta_modello.R B_i $1 &
Rscript f_scelta_modello.R B_s $1 &
Rscript f_scelta_modello.R Ba_i $1 &
Rscript f_scelta_modello.R Ba_s $1 &
wait

Rscript f_scelta_modello.R Bi_i $1 &
Rscript f_scelta_modello.R Bi_s $1 &
Rscript f_scelta_modello.R Ca_i $1 &
Rscript f_scelta_modello.R Ca_s $1 &
Rscript f_scelta_modello.R Cd_i $1 &
Rscript f_scelta_modello.R Cd_s $1 &
Rscript f_scelta_modello.R Ce_i $1 &
wait

Rscript f_scelta_modello.R Ce_s $1 &
Rscript f_scelta_modello.R Co_i $1 &
Rscript f_scelta_modello.R Co_s $1 &
Rscript f_scelta_modello.R Cr_i $1 &
Rscript f_scelta_modello.R Cr_s $1 &
Rscript f_scelta_modello.R Cs_i $1 &
Rscript f_scelta_modello.R Cs_s $1 &
Rscript f_scelta_modello.R Cu_i $1 &
wait

Rscript f_scelta_modello.R Cu_s $1 &
Rscript f_scelta_modello.R Fe_i $1 &
Rscript f_scelta_modello.R Fe_s $1 &
Rscript f_scelta_modello.R Ga_i $1 &
Rscript f_scelta_modello.R Ga_s $1 &
Rscript f_scelta_modello.R K_i $1 &
Rscript f_scelta_modello.R K_s $1 &
Rscript f_scelta_modello.R La_i $1 &
wait

Rscript f_scelta_modello.R La_s $1 &
Rscript f_scelta_modello.R Li_i $1 &
Rscript f_scelta_modello.R Li_s $1 &
Rscript f_scelta_modello.R Mg_i $1 &
Rscript f_scelta_modello.R Mg_s $1 &
Rscript f_scelta_modello.R Mn_i $1 &
Rscript f_scelta_modello.R Mn_s $1 &
Rscript f_scelta_modello.R Mo_i $1 &
wait

Rscript f_scelta_modello.R Mo_s $1 &
Rscript f_scelta_modello.R Na_i $1 &
Rscript f_scelta_modello.R Na_s $1 &
Rscript f_scelta_modello.R Nb_i $1 &
Rscript f_scelta_modello.R Nb_s $1 &
Rscript f_scelta_modello.R Ni_i $1 &
Rscript f_scelta_modello.R Ni_s $1 &
Rscript f_scelta_modello.R PM10 $1 &
wait

Rscript f_scelta_modello.R Pb_i $1 &
Rscript f_scelta_modello.R Pb_s $1 &
Rscript f_scelta_modello.R Rb_i $1 &
Rscript f_scelta_modello.R Rb_s $1 &
Rscript f_scelta_modello.R Sb_i $1 &
Rscript f_scelta_modello.R Sb_s $1 &
Rscript f_scelta_modello.R Sn_i $1 &
Rscript f_scelta_modello.R Sn_s $1 &
wait

Rscript f_scelta_modello.R Sr_i $1 &
Rscript f_scelta_modello.R Sr_s $1 &
Rscript f_scelta_modello.R Ti_i $1 &
Rscript f_scelta_modello.R Ti_s $1 &
Rscript f_scelta_modello.R Tl_i $1 &
Rscript f_scelta_modello.R Tl_s $1 &
Rscript f_scelta_modello.R U_i $1 &
Rscript f_scelta_modello.R U_s $1 &
wait

Rscript f_scelta_modello.R V_i $1 &
Rscript f_scelta_modello.R V_s $1 &
Rscript f_scelta_modello.R W_i $1 &
Rscript f_scelta_modello.R W_s $1 &
Rscript f_scelta_modello.R Zn_i $1 &
Rscript f_scelta_modello.R Zn_s $1 &
Rscript f_scelta_modello.R Zr_i $1 &
Rscript f_scelta_modello.R Zr_s $1 &
wait

