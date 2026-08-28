!======================================================================!
subroutine SOILTEMP
!----------------------------------------------------------------------!
! based on 1D FTCS Heat Diffusion Equation
! Timestep dt_hr = 1800 s (30 min)
! Hannah 6/8/26
!----------------------------------------------------------------------!
use PARS_MOD
use VARS_MOD
!----------------------------------------------------------------------!
implicit none
!----------------------------------------------------------------------!
! Dimensionless mixing factor: r = D * dt / dz^2
!----------------------------------------------------------------------!
mixing_factor = (D_soil * dt_hr) / (dz_soil ** 2)
!----------------------------------------------------------------------!
! Upper Boundary Condition at z = 0
! Empirical scaling of air temperature for soil surface temperature
! T[t, 0] = mean_air_temp + (a * air_temp_fluctuation) + b
!----------------------------------------------------------------------!
air_temp_fluctuation = TC - mean_air_temp
T_new(1) = mean_air_temp + (param_a * air_temp_fluctuation) + param_b
!----------------------------------------------------------------------!
! Interior Node Finite Difference (Central Difference in Space)
! T[t, z] = T[last, z] + mixing_factor * (T[last, z+1] - 2*T[last, z]
!                                                        + T[last, z-1])
!----------------------------------------------------------------------!
do its = 2, nz - 1
   T_new(its) = T_profile(its) + mixing_factor * &
            (T_profile(its+1) - 2.0 * T_profile(its) + T_profile(its-1))
end do
!----------------------------------------------------------------------!
! Lower Boundary Condition at z = z_bottom (Node nz)
! Approximates a zero-flux condition
! T[t, Nz-1] = T[last, Nz-1] + mixing_factor * (T[last, Nz-2]
!                                                       - T[last, Nz-1])
!----------------------------------------------------------------------!
T_new(nz) = T_profile(nz) + mixing_factor * (T_profile(nz - 1) &
                                                        - T_profile(nz))
!----------------------------------------------------------------------!
! Update state vector for the next timestep
!----------------------------------------------------------------------!
T_profile(:) = T_new(:)
!----------------------------------------------------------------------!
! Depth-integrate
!----------------------------------------------------------------------!
sum_temp_1 = 0.0
do its = 1, n_layer1_nodes
  sum_temp_1 = sum_temp_1 + T_profile(its)
end do

sum_temp_2 = 0.0
do its = n_layer1_nodes + 1, n_layer2_nodes
  sum_temp_2 = sum_temp_2 + T_profile(its) 
end do

T_soil (1) = sum_temp_1 / real(n_layer1_nodes)
T_soil (2) = sum_temp_2 / real(n_layer2_nodes - n_layer1_nodes)
!----------------------------------------------------------------------!
end subroutine SOILTEMP
!======================================================================!
