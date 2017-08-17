module Global_variables

	implicit none
	save
	public

	integer, parameter				:: nx = 322, ny = 242		  	! Grid size
	!character(len=*), parameter     		:: Analyse_dir = '/home/sfr009/PHD/Testing/Test_enkf-c/ensemble_6565/' ! Global directory 
	!character(len=*), parameter     		:: Restart_dir = '/home/sfr009/PHD/Testing/Test_enkf-c/Restart_out/' ! Global directory

	!character(len=*), parameter     		:: Obs_dir = '/work/sfr009/' ! Global directory

	integer, parameter				:: ens_size = 10




end module Global_variables
