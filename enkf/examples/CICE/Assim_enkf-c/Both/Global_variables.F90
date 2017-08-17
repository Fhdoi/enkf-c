module Global_variables

	implicit none
	save
	public

	integer, parameter				:: nx = 322, ny = 242		  	! Grid size
	character(len=*), parameter     		:: Analyse_dir = '/global/work/sfr009/Rundir_enkf-c/Assim_enkf-c/ensemble_6565/' ! 
	character(len=*), parameter     		:: Restart_dir = '/global/work/sfr009/Rundir_enkf-c/' ! 

	integer, parameter				:: ens_size = 10

	contains

	subroutine get_date(date)


			character(len=550)						:: Date_full
			character(len=500)						:: date_temp
			character(len=30), intent(out)					:: date
			
			Date_full = Restart_dir // 'Ens1/restart/ice.restart_file'
			OPEN(UNIT=1,FILE='test2',FORM="UNFORMATTED", STATUS="OLD",ACTION="READ")
			READ(1) date_temp
			CLOSE(UNIT=1)
			print*, date_temp
			date = date_temp(17:32)
			
	end subroutine get_date	

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

end module Global_variables
