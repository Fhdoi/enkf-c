program Consistency

	use netcdf
	use mpi

	use read_write_nc, only: create_res_nc, read_var_nc, write_var_nc
	use Global_variables, only: nx,ny,ens_size

	implicit none

	integer								:: i,j,k
	character(len=1) 					:: str
	character(len=2) 					:: str2
	character(len=3) 					:: str3
	integer								:: member
	character(len=50)					:: file_name
	real, dimension(nx,ny)				:: aicen_sum
	real, dimension(nx,ny,ens_size)		:: aicen_res_before, aicen_res_after
	real, dimension(nx,ny,5)			:: aicen_temp
	character(len=*),parameter			:: file_out = '../aicen_res.nc'	

	call create_res_nc(file_out)

	do member = 1,ens_size

		if(member < 10) then
			write( str, '(I1)' ) member
			str3 = '00' // str
		elseif (member > 9 .and. member < 100) then
			write( str2 , '(I2)' ) member
			str3 = '0' // str2
		else
			write( str3 , '(I3)' ) member

		endif
	
		file_name = '../ensemble_6565/mem' // str3 // '_aicen.nc'
		call read_var_nc(file_name, 'aicen', 5, 3, aicen_temp)

		do i = 1,nx
			do j = 1,ny
				aicen_sum(i,j) = 0
				do k = 1,5
					aicen_sum(i,j) = aicen_sum(i,j) + aicen_temp(i,j,k)
				enddo
			enddo
		enddo

		aicen_res_before(:,:,member) = aicen_sum
	
		if (member == 1) then
			call write_var_nc(file_out, 'mem001_before', 5, 3, aicen_temp)
		endif

		file_name = '../ensemble_6565/mem' // str3 // '_aicen.nc.analysis'
		call read_var_nc(file_name, 'aicen', 5, 3, aicen_temp)

		do i = 1,nx
			do j = 1,ny
				aicen_sum(i,j) = 0
				do k = 1,5
					aicen_sum(i,j) = aicen_sum(i,j) + aicen_temp(i,j,k)
				enddo
			enddo
		enddo

		aicen_res_after(:,:,member) = aicen_sum

		if (member == 1) then
			call write_var_nc(file_out, 'mem001_after', 5, 3, aicen_temp)
		endif

	enddo


	call write_var_nc(file_out, 'Ens_before', ens_size, 3, aicen_res_before)
	call write_var_nc(file_out, 'Ens_after', ens_size, 3, aicen_res_after)
	

end program Consistency
