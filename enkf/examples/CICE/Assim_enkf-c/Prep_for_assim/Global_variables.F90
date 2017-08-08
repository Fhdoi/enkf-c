module Global_variables

	implicit none
	save
	public

	integer, parameter				:: nx = 322, ny = 242		  	! Grid size 
	character(len=*), parameter     		:: Ensemble_dir = '/home/sfr009/PHD/EnKF-C_standalone/Assim_enkf-c/ensemble_6565/' ! 
	character(len=*), parameter     		:: Restart_dir = '/home/sfr009/PHD/Testing/Rundir_ens_test/' ! 

	integer, parameter				:: ens_size = 10


	contains
	subroutine cat_sum(Var, zdim, Varsum)

        integer, intent(in)             :: zdim	
	real, intent(in)		:: Var(nx,ny,zdim)
	real, intent(out)		:: Varsum(nx,ny)

	integer					:: i,j,k

	do i = 1,nx
		do j = 1,ny
			Varsum(i,j) = 0
			do k = 1,zdim
				Varsum(i,j) = Varsum(i,j) + Var(i,j,k)
			enddo
		enddo
	enddo
	


	end subroutine cat_sum

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
