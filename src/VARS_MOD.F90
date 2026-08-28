module VARS_MOD
use PARS_MOD
implicit none
integer :: kt_6hr
real    :: tmp_global_dummy, pre_global_dummy, tswrf_global_dummy
real    :: dlwrf_global_dummy, spfh_global_dummy
real    :: pres_global_dummy, ugrd_global_dummy
character (len=200) :: home_dir
character (len=  4) :: cyr
character (len=200) :: filename
integer, dimension (nland) :: x_k, y_k
integer :: syr
integer :: eyr
integer :: nyr_sim
integer :: nyr_co2
integer :: kyr_ce
integer :: ikyr
integer :: kday
integer :: kt
integer :: it
integer :: ihr
integer :: ic_count
integer :: ivar
integer :: k
integer :: kw
integer :: kl
integer :: ip ! SOM pool index (pool no.)
real, dimension (nlon) :: lon ! Longitude (degrees east)
real, dimension (nlat) :: lat ! Latitude (degrees north)
real, dimension (n_pools) :: decay, k_decay, c_start, input_vec
real, dimension (n_pools) :: transfer_vec, c_end
real, dimension (n_pools,nlayers) :: pool_initial, c_state
real, dimension (nlayers) :: total_litter_day, total_input
real, allocatable :: co2_ppm (:)
real :: fiSOM
real :: hr
real, allocatable :: tmp   (:,:)
real, allocatable :: pre   (:,:)
real, allocatable :: tswrf (:,:)
real, allocatable :: dlwrf (:,:)
real, allocatable :: spfh  (:,:)
real, allocatable :: pres  (:,:)
real, allocatable :: ugrd  (:,:)
real, allocatable :: vgrd  (:,:)
real :: tmp_l
real :: pre_l
real :: tswrf_l
real :: dlwrf_l
real :: spfh_l
real :: pres_l
real :: ugrd_l
real :: vgrd_l
!----------------------------------------------------------------------!
! For HYDRO.
!----------------------------------------------------------------------!
real :: lamb
real :: Rnet
real :: eLAI
real :: Rnets
real :: G
real :: A
real :: As
real :: x
real :: dsp
real :: ras_alpha
real :: raa_alpha
real :: ras_0
real :: raa_0
real :: gHR_closed
real :: gHR_bare
real :: PMw
real :: PMc
real :: PMs
real :: Ra, Rs, Rc
real :: Cc, Cs
real :: LEc_bulk, LEs
real :: Wcan
real :: pot_Wcan
real :: qinmax
real :: qflx_infl
real :: qflx_infl_excess
real :: qflx_in_soil_local
real :: qflx_can
real :: evap_can_surface
real :: qflx_prec_grnd_rain
real :: aet_soil, aet_surf
real :: drip
real :: rain
real :: snow
real :: ftop
real :: froot_top
real :: W0, W1
real :: C0, C1
real :: Cbal, Wbal
real :: ddf
real :: melt
real :: snowpack
real :: height
real, allocatable :: T_soil (:) ! oC
real, dimension (2) :: SM_MIN ! Min. soil water storage in layer (mm)
real, dimension (2) :: SM_MAX ! Max. soil water storage in layer (mm)
real, dimension (2) :: sm
real, dimension (2) :: rwc    ! Soil water relative to
real, dimension (2) :: swc    ! Soil water relative to
real, dimension (2) :: dz     ! Layer thickness (mm)
real, dimension (2) :: theta  ! Volumetric soil water (mm/mm)
real, dimension (2) :: dsm
real, dimension (2) :: sm_day ! Mean daily soil moisture (mm)
!----------------------------------------------------------------------!
real :: D0
real :: D_mol
real :: D_mbar
real :: swp
real :: rho_mol
real :: RT_air
real :: pcp
real :: Jmax_T
real :: Vcmax_T
real :: Kc
real :: Ko
real :: es
real :: ea
real :: Q_top
real :: TC
real :: PPT
real :: Vcmax_l
real :: Jmax_l
real :: Q_l
real :: Rd_leaf_l
real :: gs_leaf_a , Ag_leaf_a , Rd_leaf_a
real :: gs_leaf_ab, Ag_leaf_ab, Rd_leaf_ab
real :: gs_leaf_b , Ag_leaf_b , Rd_leaf_b
real :: gs_crown  , Ag_crown  , Rd_crown
real :: Ksoil
real :: Ktot
real :: f0
real :: km
real :: gamma_m
real :: ZCAP
real :: x_CAP_V
real :: w_CAP
real :: a_CAP
real :: ca_fmol
real :: gs_leaf_V
real :: Jelec
real :: x_CAP_J
real :: gs_leaf_J
real :: ci
real :: x_CAP
real :: scale
real :: LAI
real :: Abot
real :: raa1
real :: raa2
real :: raa3
real :: raa4
real :: raa5
real :: sm_q
real :: perc
real :: Delta
real :: rho_kg
real :: h
real :: xh
real :: z0
real :: u
real :: ras
real :: gamma
real :: rss
real :: rr
real :: rhr
real :: LE
real :: raa
real :: rac
real :: rsc
real :: aet
real :: PPT_day
real :: GPP_day
real :: Raut_day
real :: Rhet_day
real :: GPP_ann
real :: PPT_ann
real :: RO_ann
real :: ET_ann
real :: l, rt, t, b
real :: tmod
real :: LE_day ! Mean daily latent heat flux (W/m2)
real :: TC_day ! Mean daily temperature (oC)
real :: denom
real :: wfps
real :: wmod
real :: amod
real :: texture_modifier
real :: fm_shoot, fm_root
real :: shoot_input, root_input
real :: co2
real :: silt_plus_clay, ft, cal, cap, cas, total
real :: leaching_water_cm, leaching_water_day, leaching_cm_day, leached_c
real :: csp
real :: Raut ! Autotrophic respiration (g[C] m-2 s-1)
real :: Rhet ! Heterotrophic respiration (g[C] m-2 s-1)
real :: gpp ! g[C] m-2 s-1
real :: npp ! g[DM] m-2 s-1
real :: litter ! g[DM] m-2 s-1
real :: dbiomass ! g[DM] m-2 s-1
real :: biomass ! g[DM] m-2
real :: SOM ! g[SOM] m-2
real :: L_ann ! g[C] m-2 yr-1
real :: Raut_ann ! g[C] m-2 yr-1
real :: Rhet_ann ! g[C] m-2 yr-1
real :: NEE_ann ! g[C] m-2 yr-1
!!
real :: NEE
real :: NEE_day
real :: G_day
integer :: its
real :: TC_sum
real :: sm_sum
real, dimension (2) :: T_soil_daily_mean
real, dimension (2) :: T_soil_daily_sum
real :: depth_layer1
real :: depth_layer2
integer :: n_layer1_nodes 
integer :: n_layer2_nodes 
real :: TC_daily_mean
real :: sm_daily_mean
real :: T_soil_new
real :: mixing_factor
real :: sum_temp_1
real :: sum_temp_2
real :: air_temp_fluctuation
real, dimension(nz) :: T_new
real, dimension(nz) :: T_profile
real :: RECO_day
!!
real, allocatable, dimension (:,:) :: tmp_global   ! K
real, allocatable, dimension (:,:) :: pre_global   ! mm/6-hr
real, allocatable, dimension (:,:) :: tswrf_global ! W m-2
real, allocatable, dimension (:,:) :: dlwrf_global ! W m-2
real, allocatable, dimension (:,:) :: spfh_global  ! kg[water] kg[air]-1
real, allocatable, dimension (:,:) :: pres_global  ! Pa
real, allocatable, dimension (:,:) :: ugrd_global  ! m s-1
real, allocatable, dimension (:,:) :: vgrd_global  ! m s-1
end module VARS_MOD
