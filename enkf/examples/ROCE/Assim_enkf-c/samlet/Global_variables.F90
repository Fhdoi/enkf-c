module Global_variables

	implicit none
	save
	public

	integer, parameter				:: nx = 322, ny = 242		  	! Grid size 
	character(len=*), parameter     		:: Ensemble_dir = '/global/work/sfr009/Coupled_Pert/Assim_enkf-c/ensemble_6565/' ! Global directory
	character(len=*), parameter     		:: Restart_dir = '/global/work/sfr009/Coupled_Results/Coupled_Pert/' ! Global directory

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

end module Global_variables
