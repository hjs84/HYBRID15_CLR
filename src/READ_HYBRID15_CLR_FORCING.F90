subroutine read_HYBRID15_CLR_forcing
!----------------------------------------------------------------------!
use PARS_MOD
use VARS_MOD
!----------------------------------------------------------------------!
implicit none
!----------------------------------------------------------------------!
open (10,file='/home/hjs84/rds/rds-geogscratch-bx5LSFBEVHM/adf10/&
&TRENDYGCB2026/CO2field/global_co2_ann_1700_2025.txt',status='old')
do kyr_ce = 1700, eyr
  read (10,*) ikyr, co2_ppm (kyr_ce)
end do
close (10)
!----------------------------------------------------------------------!
open (77, file = 'UK-HgF_HH_forcing_co2.txt', status = 'old')
!----------------------------------------------------------------------!
ikyr = 1
do kyr_ce = syr, eyr
  write (*,*) 'Reading forcing data for year: ', kyr_ce
  write (cyr,'(i4)') kyr_ce
  do kt = 1, ntimes
    kt_6hr = ((kt - 1) / 12) + 1
    read(77, *) &
      tmp   (ikyr, kt), &  ! Column 1: Temperature (K)
      pre   (ikyr, kt), &  ! Column 2: Precipitation (mm/s)
      tswrf (ikyr, kt), &  ! Column 3: Shortwave In (W/m2)
      dlwrf (ikyr, kt), &  ! Column 4: Longwave In (W/m2)
      spfh  (ikyr, kt), &  ! Column 5: Specific Humidity (kg/kg)
      pres  (ikyr, kt), &  ! Column 6: Pressure (Pa)
      ugrd  (ikyr, kt)     ! Column 7: Wind Speed (m/s) passed as scalar
    !------------------------------------------------------------------!
    vgrd  (ikyr, kt) = 0.0
    !------------------------------------------------------------------!
  end do !kt
  !--------------------------------------------------------------------!
  ikyr = ikyr + 1
  !--------------------------------------------------------------------!
end do !ikyr
!----------------------------------------------------------------------!
close (77)
!----------------------------------------------------------------------!
end subroutine read_HYBRID15_CLR_forcing
