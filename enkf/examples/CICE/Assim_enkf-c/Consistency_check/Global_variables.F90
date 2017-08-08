module Global_variables

	implicit none
	save
	public

	integer, parameter				:: nx = 322, ny = 242		  	! Grid size
	character(len=*), parameter     		:: Analyse_dir = '/home/sfr009/PHD/EnKF-C_standalone/Assim_enkf-c/ensemble_6565/' ! 
	character(len=*), parameter     		:: Restart_dir = '/home/sfr009/PHD/Testing/Rundir_ens_test/' ! 

	integer, parameter				:: ens_size = 10

	contains

	subroutine get_date(date)


			character(len=150)						:: Date_full
			character(len=100)						:: date_temp
			character(len=30), intent(out)					:: date
			
			Date_full = Restart_dir // 'Ens1/restart/ice.restart_file'
			OPEN(UNIT=1,FILE=Date_full,FORM="UNFORMATTED", STATUS="OLD",ACTION="READ")
			READ(1) date_temp
			CLOSE(UNIT=1)
			date = date_temp(17:32)
			
	end subroutine get_date	

end module Global_variables
