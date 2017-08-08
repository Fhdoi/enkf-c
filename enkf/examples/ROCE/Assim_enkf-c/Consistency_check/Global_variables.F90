module Global_variables

	implicit none
	save
	public

	integer, parameter				:: nx = 322, ny = 242		  	! Grid size
	character(len=*), parameter     		:: Analyse_dir = '/global/work/sfr009/Coupled_Pert/Assim_enkf-c/ensemble_6565/' ! Global directory 
	character(len=*), parameter     		:: Restart_dir = '/global/work/sfr009/Coupled_Results/Coupled_Pert/' ! Global directory

	integer, parameter				:: ens_size = 10




end module Global_variables
