       MODULE RUN_PARAMETERS

       IMPLICIT NONE

       PUBLIC :: read_in_run_parameters_from_unit_5

       SAVE

      CHARACTER*100 GT_input_dataset , GT_output_dataset
      LOGICAL sw_electro
      character*100 GIP_input_dataset , GIP_output_dataset
      character*100 GIP_Apex_coords_static_file
      character*100 static_file_location
      logical sw_External_model_provides_NO_N4S_densities
      logical sw_External_model_provides_low_lat_E_fields
      logical sw_input_Auroral_production_is_single_rate
      REAL*8 ampl22 , ampl11 , ampl25
      REAL*8 ampl23 , ampl24
      REAL*8 lt22,lt23,lt24,lt11,lt25
      REAL*8 tmpmin
      REAL*8 windmx
      INTEGER i_smoothing_frequency ,  &
              i_neutral_composition_calling_frequency , &
              i_total_no_days , i_graphics_out_start 
      INTEGER iout_high
      INTEGER ipint_high
      INTEGER nnstop , nnstrt
      INTEGER nday
      INTEGER GT_timestep_in_seconds
      INTEGER GIP_calling_frequency
      REAL*8  f107


      CONTAINS



      subroutine read_in_run_parameters_from_unit_5

      IMPLICIT NONE
 

      READ(5,89001) GT_input_dataset
      READ(5,89001) GT_output_dataset
89001 FORMAT (A)

      READ(5,29001) GIP_input_dataset
      READ(5,29001) GIP_output_dataset
      READ(5,29001) static_file_location
!      GIP_Apex_coords_static_file = TRIM(static_file_location)//'GIP_dipole_coords'
      GIP_Apex_coords_static_file = TRIM(static_file_location)//'GIP_COORDS_FILE'
29001     FORMAT (A) 
  
99007 FORMAT (L1)
      READ(5,99007) sw_electro
      READ(5,29007) sw_External_model_provides_NO_N4S_densities
      READ(5,29007) sw_External_model_provides_low_lat_E_fields
      READ(5,29007) sw_input_Auroral_production_is_single_rate
29007     FORMAT (L1)

      READ(5,*) nday
      READ(5,*) f107
 
      READ(5,*) GT_timestep_in_seconds
      READ(5,*) GIP_calling_frequency

      READ(5,*) nnstrt
      READ(5,*) nnstop

      READ(5,*) i_total_no_days
      READ(5,*) i_graphics_out_start
      READ(5,*) ipint_high

      READ(5,*) i_smoothing_frequency
      READ(5,*) i_neutral_composition_calling_frequency
      READ(5,*) windmx
      READ(5,*) tmpmin

      READ(5,*) ampl11,ampl22 , ampl23 , ampl24 , ampl25
      READ(5,*) lt11 , lt22 , lt23 , lt24 , lt25

      return

      end subroutine read_in_run_parameters_from_unit_5


      END MODULE RUN_PARAMETERS
