!======================================================================!
subroutine CROWN
!----------------------------------------------------------------------!
! Environmental forcings:
! tswrf_l
! pres_l
! tmp_l
! TC
! spfh_l
! sm
! Other forcings:
! LAI
!----------------------------------------------------------------------!
use PARS_MOD
use VARS_MOD
!----------------------------------------------------------------------!
implicit none
!----------------------------------------------------------------------!
! For testing.
!tswrf_l = 900.0
!pres_l = 101325.0
!tmp_l = 298.0
!TC = tmp_l - tf
!spfh_l = 0.015
!sm = sm_max
!LAI = 8.0
!----------------------------------------------------------------------!
! Downwelling photosynthetically-active radiation at top of canopy
!                                                   mol[photons] m-2 s-1
!----------------------------------------------------------------------!
Q_top   = mol_per_J * tswrf_l
!----------------------------------------------------------------------!
! Air density                                               mol[air] m-3
!----------------------------------------------------------------------!
rho_mol = pres_l / (R * tmp_l)
!----------------------------------------------------------------------!
! Intermediate variable                                     J mol[air]-1
!----------------------------------------------------------------------!
RT_air  = R * tmp_l
!----------------------------------------------------------------------!
! Photorespiratory compensation point, bernacchi01   mol[CO2] mol[air]-1
!----------------------------------------------------------------------!
pcp     = exp (19.02 - 37830.0 / RT_air) / 1.0e6
!----------------------------------------------------------------------!
! Maximum electron transport rate temperature modifier            scalar
!----------------------------------------------------------------------!
Jmax_T  = exp (-(((TC - Topt_J) / omega_J) ** 2))
!----------------------------------------------------------------------!
! Maximum carboxylation rate temperature modifier                 scalar
!----------------------------------------------------------------------!
Vcmax_T = exp (26.35 - 65330.0 / RT_air) !bernacchi01
!----------------------------------------------------------------------!
! Michaelis-Menten constant for carboxylation          mol[CO2[ mol[air]
!----------------------------------------------------------------------!
Kc = exp (38.05 - 79430.0 / RT_air) / 1.0e6 !bernacchi01
!----------------------------------------------------------------------!
! Michaelis-Menten constant for oxygenation             mol[O2[ mol[air]
!----------------------------------------------------------------------!
Ko = exp (20.30 - 36380.0 / RT_air) / 1.0e3 !bernacchi01
!----------------------------------------------------------------------!
! Saturated water vapour pressure                                     Pa
! Tetens equation, Google AI; closish to jones new table.
!----------------------------------------------------------------------!
es = 611.2 * exp ((17.67 * TC) / (TC + 243.5))
!----------------------------------------------------------------------!
! Actual vapour pressure, Google AI                                   Pa
!----------------------------------------------------------------------!
!ea = (0.622 * spfh * pres) / (one - 0.378 * spfh)
ea = spfh_l * pres_l / (0.622 + 0.378 * spfh_l) ! Thanks Hannah!
!----------------------------------------------------------------------!
! Atmospheric water vapour pressure deficit                           Pa
!----------------------------------------------------------------------!
D0 = es - ea
!----------------------------------------------------------------------!
! Atmospheric water vapour pressure deficit        mol[water] mol[air]-1
!----------------------------------------------------------------------!
D_mol = D0 / pres_l
!----------------------------------------------------------------------!
! Atmospheric water vapour pressure deficit                         mbar
!----------------------------------------------------------------------!
D_mbar = D0 / 100.0
!----------------------------------------------------------------------!
! Relative water content in each layer                          fraction
!----------------------------------------------------------------------!
do kl = 1, nlayers
  !--------------------------------------------------------------------!
  rwc (kl) = (sm (kl) - SM_MIN (kl)) / (SM_MAX (kl) - SM_MIN (kl))
  rwc (kl) = min (one, rwc (kl))
  rwc (kl) = max (eps, rwc (kl))
  !--------------------------------------------------------------------!
end do ! kl
!----------------------------------------------------------------------!
! Soil water potential                                               MPa
!----------------------------------------------------------------------!
if (rwc (1) > rwc (2)) then
  swp = swp_max * (one / (rwc (1) ** bsoil)) ! friend95
else
  swp = swp_max * (one / (rwc (2) ** bsoil)) ! friend95
endif
!----------------------------------------------------------------------!
! Inhibition due to high soil water.
!----------------------------------------------------------------------!
fsat = 0.0
do kl = 1, nlayers
  wfps = 100.0 * theta (kl) / theta_sat
  if (wfps > wfps_threshold) then
    wmod (kl) = exp (((wfps - wfps_threshold) ** 2) / (-moisture_dry_width))
    wmod (kl) = zero
  else
    wmod (kl) = one
  endif
  fsat = fsat + froot (kl) * wmod (kl)
end do
!----------------------------------------------------------------------!
! Values at top of crown.
!----------------------------------------------------------------------!
Vcmax_l = fsat * Vcmax_T * Vcmax_top
Jmax_l = fsat * Jmax_T * Jmax_top
Q_l = Q_top
!----------------------------------------------------------------------!
! Compute physiology for this level in crown.
!----------------------------------------------------------------------!
call leaf (Vcmax_l, Jmax_l, Q_l, Rd_leaf_l, gs_leaf_a, Ag_leaf_a, &
           Rd_leaf_a)
!----------------------------------------------------------------------!
scale = exp (-KPh * LAI / 2.0)
Vcmax_l = scale * Vcmax_T * Vcmax_top
Jmax_l = scale * Jmax_T * Jmax_top
Q_l = exp (-KPAR * LAI / 2.0) * Q_top
!----------------------------------------------------------------------!
call leaf (Vcmax_l, Jmax_l, Q_l, Rd_leaf_l, gs_leaf_ab, Ag_leaf_ab, &
           Rd_leaf_ab)
!----------------------------------------------------------------------!
scale = exp (-KPh * LAI)
Vcmax_l = scale * Vcmax_T * Vcmax_top
Jmax_l = scale * Jmax_T * Jmax_top
Q_l = exp (-KPAR * LAI) * Q_top
!----------------------------------------------------------------------!
call leaf (Vcmax_l, Jmax_l, Q_l, Rd_leaf_l, gs_leaf_b, Ag_leaf_b, &
           Rd_leaf_b)
!----------------------------------------------------------------------!
gs_crown = (LAI / 6.0) * (gs_leaf_a + 4.0 * gs_leaf_ab + gs_leaf_b)
Ag_crown = (LAI / 6.0) * (Ag_leaf_a + 4.0 * Ag_leaf_ab + Ag_leaf_b)
Rd_crown = (LAI / 6.0) * (Rd_leaf_a + 4.0 * Rd_leaf_ab + Rd_leaf_b)
!----------------------------------------------------------------------!
if (Q_top > zero) Abot = Ag_leaf_b
!----------------------------------------------------------------------!
gpp = MC * Ag_crown
!----------------------------------------------------------------------!
end subroutine CROWN
!======================================================================!
