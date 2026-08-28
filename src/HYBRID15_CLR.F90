!======================================================================!
program HYBRID15_CLR
!----------------------------------------------------------------------!
! Code to simulate NEE using process-based photosynthesis, respiration,
! and soil decomposition approaches.
! Next add substrate evaporation.
!----------------------------------------------------------------------!
use PARS_MOD
use VARS_MOD
!----------------------------------------------------------------------!
implicit none
!----------------------------------------------------------------------!
write (*,*)
write (*,*) 'HYBRID15_CLR running'
write (*,*)
!----------------------------------------------------------------------!
! Read forcings and initalise all state variables etc.
!----------------------------------------------------------------------!
write (*,*) 'Starting INIT'
call INIT
!----------------------------------------------------------------------!
T_profile (:) = mean_air_temp
T_soil_daily_mean (:) = mean_air_temp
T_soil (:) = mean_air_temp
!----------------------------------------------------------------------!
kyr_ce = syr
do ikyr = 1, nyr_sim
  C0 = CDM * biomass + sum (c_state)
  W0 = sm (1) + sm (2) + Wcan
  ca_fmol = co2_ppm (kyr_ce) / 1.0e6 ! mol[CO2] mol[air]-1
  Raut_ann = zero ! Annual autotrophic respiration       (g[C] m-2 yr-1)
  GPP_ann  = zero ! Annual gross primary production      (g[C] m-2 yr-1)
  Rhet_ann = zero ! Annual heterotrophic respiration     (g[C] m-2 yr-1)
  L_ann    = zero ! Annual litter flux (g[C] m-2 yr-1)
  PPT_ann  = zero ! Annual precipitation                         (mm/yr)
  RO_ann   = zero
  ET_ann   = zero
  NEE_ann  = zero
  it = 0
  do kday = 1, ndays
    hr = 0.0 - dt_hr
    TC_day   = zero
    LE_day   = zero
    sm_day   = zero
    PPT_day  = zero
    GPP_day  = zero
    Raut_day = zero
    total_litter_day = zero
    leaching_water_day = zero
    G_day = zero
    T_soil_daily_sum = zero
    do kt = 1, nt
      !----------------------------------------------------------------!
      it = it + 1
      !----------------------------------------------------------------!
      hr = hr + dt_hr
      !----------------------------------------------------------------!
      ! Set local climate variables for this timepoint.
      !----------------------------------------------------------------!
      tmp_l   = tmp   (ikyr,it) ! Air temperature                    (K)
      TC      = tmp_l - tf      ! Air temperature                   (oC)
      pre_l   = pre   (ikyr,it) ! Precipitation                   (mm/s)
      tswrf_l = tswrf (ikyr,it) ! Tot dnwd SW flx, sfc, time mean (W/m2)
      dlwrf_l = dlwrf (ikyr,it) ! Dnwd LW rad flx                 (W/m2)
      spfh_l  = spfh  (ikyr,it) ! Specific humidity              (kg/kg)
      pres_l  = pres  (ikyr,it) ! Pressure                          (Pa)
      ugrd_l  = ugrd  (ikyr,it) ! Zonal component of wnd speed     (m/s)
      vgrd_l  = vgrd  (ikyr,it) ! Merdional component of wnd speed (m/s)
      !----------------------------------------------------------------!
      TC_day = TC_day + TC
      !----------------------------------------------------------------!
      write (20, '(3i6, f6.1, 8f12.4, 15f16.10)') kyr_ce, kday, kt, &
            hr, T_soil(1), T_soil(2), T_soil_daily_mean(1), &
            T_soil_daily_mean(2), tmp_l, co2_ppm(kyr_ce), sm(1), &
            sm(2), gpp, Raut, aet, G, Rhet, npp, pre_l, sm_q, gpp_day, &
            Raut_day, Rhet_day, biomass, LE, TC, perc
      !----------------------------------------------------------------!
      ! Compute crown photosynthesis, respiration, and conductance.
      !----------------------------------------------------------------!
      call CROWN
      !----------------------------------------------------------------!
      ! Advance soil hydrology.
      !----------------------------------------------------------------!
      call HYDRO
      call SOILTEMP
      !----------------------------------------------------------------!
      ! Advance biomass.
      !----------------------------------------------------------------!
      call GROW
      !----------------------------------------------------------------!
      ! g[DM] m-2 day-1
      !----------------------------------------------------------------!
      total_litter_day (1) = total_litter_day (1) + dt_s * litter
      G_day = G_day + dt_s * G
      !----------------------------------------------------------------!
      ! Water leaching in day (cm day-1)
      !----------------------------------------------------------------!
      leaching_water_day = leaching_water_day + leaching_water_cm
      !----------------------------------------------------------------!
      ! Mean daily soil moisture (mm)
      !----------------------------------------------------------------!
      do kl = 1, nlayers
        sm_day (kl) = sm_day (kl) + sm (kl)
        T_soil_daily_sum (kl) = T_soil_daily_sum (kl) + T_soil (kl)
      end do
      !----------------------------------------------------------------!
      ! Mean daily latent heat flux (mm)
      !----------------------------------------------------------------!
      LE_day   = LE_day   + LE
      !----------------------------------------------------------------!
      ! Accumulate daily diagnostics.
      !----------------------------------------------------------------!
      PPT_day  = PPT_day  + dt_s * pre_l
      GPP_day  = GPP_day  + dt_s * gpp
      Raut_day = Raut_day + dt_s * Raut
      !----------------------------------------------------------------!
      ! Accumulate annual diagnostics.
      !----------------------------------------------------------------!
      PPT_ann  = PPT_ann  + dt_s * pre_l
      RO_ann   = RO_ann   + dt_s * sm_q
      ET_ann   = ET_ann   + dt_s * aet
      GPP_ann  = GPP_ann  + dt_s * gpp
      Raut_ann = Raut_ann + dt_s * Raut
      NEE_ann  = NEE_ann  + dt_s * (Raut - gpp)
      !----------------------------------------------------------------!
    end do ! kt
    !------------------------------------------------------------------!
    ! Daily total plant C input to soil decomposition routine.
    !------------------------------------------------------------------!
    total_input = CDM * total_litter_day
    !------------------------------------------------------------------!
    ! Mean daily temperature (oC)
    !------------------------------------------------------------------!
    TC_day = TC_day / float (nt)
    T_soil_daily_mean (1) = T_soil_daily_sum (1) / float (nt)
    T_soil_daily_mean (2) = T_soil_daily_sum (2) / float (nt)
    !------------------------------------------------------------------!
    ! Mean daily latent heat flux (W/m2)
    !------------------------------------------------------------------!
    LE_day = LE_day / float (nt)
    G_day = G_day / float (nt)
    !------------------------------------------------------------------!
    ! Mean daily soil moisture (mm)
    !------------------------------------------------------------------!
    sm_day (:) = sm_day (:) / float (nt)
    !------------------------------------------------------------------!
    ! Daily leaching water for input to soil decomposition routine.
    !------------------------------------------------------------------!
    leaching_water_day = leaching_cm_day ! I think!
    !------------------------------------------------------------------!
    ! Advance SOM.
    !------------------------------------------------------------------!
    call DECOMP
    !------------------------------------------------------------------!
    ! Accumulate annual diagnostics.
    !------------------------------------------------------------------!
    L_ann    = L_ann    + sum (total_litter_day (:))
    Rhet_ann = Rhet_ann + day_s * Rhet
    NEE_ann  = NEE_ann  + day_s * Rhet
    !------------------------------------------------------------------!
    Rhet_day = day_s * Rhet
    NEE_day = Raut_day + Rhet_day - GPP_day
    !------------------------------------------------------------------!
    write (24,'(2i6,9f12.4)') kyr_ce, kday, NEE_day, GPP_day, &
                              Raut_day, Rhet_day, PPT_day, LE_day, &
                              G_day, sm_day (1), snowpack
    !------------------------------------------------------------------!
  end do ! kday
  !--------------------------------------------------------------------!
  write (*,'(i5,10f12.4)') kyr_ce, GPP_ann, Raut_ann, Rhet_ann, &
                           NEE_ann, L_ann, biomass, SOM, PPT_ann, &
                           RO_ann, ET_ann
  !--------------------------------------------------------------------!
  C1 = CDM * biomass + sum (c_state) ! final C
  Cbal = C1 & ! final C
                         - C0 & ! initial C
                         - (GPP_ann - Raut_ann - Rhet_ann) ! gains - losses
  write (23,*) 'Cbal = ', Cbal
  if (abs (Cbal) > 1.0) write (*,*) 'Cbal problem',Cbal
  W1 = sm (1) + sm (2) + Wcan
  Wbal = (W1-W0)-(PPT_ann-RO_ann-ET_ann)
  write (23,*) 'Wbal = ',Wbal
  if (abs(Wbal) > 1.0) write (*,*) 'Wbal problem',Wbal
  kyr_ce = kyr_ce + 1
  !--------------------------------------------------------------------!
end do ! kyr
!----------------------------------------------------------------------!
close (20)
close (23)
close (24)
call execute_command_line("awk '{$1=$1}1' OFS=, results/HYBRID15_CLR_dt_output.txt > results/HYBRID15_CLR_dt_output.csv")
call execute_command_line("awk '{$1=$1}1' OFS=, results/HYBRID15_CLR_day_output.txt > results/HYBRID15_CLR_day_output.csv")
write (*,*)
write (*,*) 'HYBRID15_CLR finished'
write (*,*)
!----------------------------------------------------------------------!
end program HYBRID15_CLR
!======================================================================!
