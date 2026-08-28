!======================================================================!
subroutine INIT
!----------------------------------------------------------------------!
use PARS_MOD
use VARS_MOD
!----------------------------------------------------------------------!
implicit none
!----------------------------------------------------------------------!
open (10, file = 'home_dir.txt', status = 'old')
read (10,*) home_dir
close (10)
!----------------------------------------------------------------------!
open (11, file = 'driver.txt', status = 'old')
read (11,*) syr
read (11,*) eyr
read (11,*) nyr_co2
read (11,*) dz (1)
read (11,*) dz (2)
read (11,*) theta (1)
read (11,*) theta (2)
read (11,*) snowpack
read (11,*) Wcan
read (11,*) LAI
read (11,*) height
read (11,*) biomass
read (11,*) froot_top
do ip = 1, n_pools
  read (11,*) pool_initial (ip,1)
end do
read (11,*) fiSOM
close (11)
!----------------------------------------------------------------------!
! Assume SM_MAX is saturated water content (porosity) and all soil is
! peat. Using Eqn. 7.90 of oleson and theta_sat_om = 0.9. But obs
! suggest 0.7, so calibrate down to that.
!----------------------------------------------------------------------!
SM_MAX (:) = theta_sat * dz (:)
SM_MIN (:) = SM_MAX / saturation_to_minimum
!----------------------------------------------------------------------!
sm (1) = theta (1) * dz (1)
sm (2) = SM_MAX (2) !theta (2) * dz (2)
depth_layer1 = dz (1) / 1000 ! Thickness of layer 1 (m)
depth_layer2 = dz (2) / 1000 ! Thickness of layer 2 (m)
n_layer1_nodes = 1 + nint(depth_layer1 / dz_soil)
n_layer2_nodes = 1 + nint((depth_layer1 + depth_layer2) / dz_soil)
write (*,*) depth_layer1, depth_layer2, n_layer1_nodes, n_layer2_nodes
!----------------------------------------------------------------------!
! Split SOM pools over layers in proportion to thicknesse.
!----------------------------------------------------------------------!
do kl = 1, nlayers
  pool_initial (:,kl) = fiSOM * pool_initial (:,1) * dz (kl) / &
                        (dz (1) + dz (2))
end do
!----------------------------------------------------------------------!
allocate (T_soil (nlayers)) ! oC
!----------------------------------------------------------------------!
if (nyr_co2 > 2025) then
  write (*,*) 'nyr_co2 = ', nyr_co2, 'exceeds input file limit of 2025'
  write (*,*) 'Stopping'
  stop
end if
!----------------------------------------------------------------------!
allocate (co2_ppm (nyr_co2))
!----------------------------------------------------------------------!
! Total number of years in simuation (yr)
!----------------------------------------------------------------------!
nyr_sim = eyr - syr + 1
!----------------------------------------------------------------------!
allocate (tmp  (nyr_sim,ntimes))
allocate (pre  (nyr_sim,ntimes))
allocate (tswrf(nyr_sim,ntimes))
allocate (dlwrf(nyr_sim,ntimes))
allocate (spfh (nyr_sim,ntimes))
allocate (pres (nyr_sim,ntimes))
allocate (ugrd (nyr_sim,ntimes))
allocate (vgrd (nyr_sim,ntimes))
!----------------------------------------------------------------------!
open (20, file = 'results/HYBRID15_CLR_dt_output.txt', status = 'unknown')
write (20,'(4A6, 8A12, 15A16)') 'kyr_ce', 'kday', 'kt', 'hr', 'T_soil_1',&
     &'T_soil_2', 'T_soilday_1', 'T_soilday_2',&
     &'tmp_l', 'co2_ppm', 'sm_1', 'sm_2', 'gpp',&
     &'Raut', 'aet', 'G', 'Rhet', 'npp', 'pre_l', 'sm_q', 'gpp_day',&
     &'Raut_day', 'Rhet_day', 'biomass', 'LE', 'TC', 'perc'
!----------------------------------------------------------------------!
open (24, file = 'results/HYBRID15_CLR_day_output.txt', status = 'unknown')
write (24,'(2A6, 9A12)') 'kyr_ce', 'kday', 'NEE_day', 'GPP_day',&
     &'Raut_day', 'Rhet_day', 'PPT_day', 'LE_day', 'G_day', 'sm_day',&
     &'snowpack'
!----------------------------------------------------------------------!
! Read all CO2 and climate forcings.
!----------------------------------------------------------------------!
call READ_HYBRID15_CLR_FORCING
!----------------------------------------------------------------------!
write (*,*)
write (*,'(a125)') 'kyr_ce    GPP_ann    Raut_ann    Rhet_ann&
               &     NEE_ann       L_ann     biomass         SOM&
               &     PPT_ann      RO_ann      ET_ann'
write (*,'(a125)') '   CE   g[C]/m/yr   g[C]/m/yr   g[C]/m/yr&
               &   g[C]/m/yr  g[DM]/m/yr    g[DM]/m2     g[C]/m2&
               &     mm yr-1    mm yr-1      mm yr-1'
!----------------------------------------------------------------------!
do kl = 1, nlayers
c_state (:,kl) = pool_initial (:,kl)
end do
!----------------------------------------------------------------------!
end subroutine INIT
!======================================================================!
