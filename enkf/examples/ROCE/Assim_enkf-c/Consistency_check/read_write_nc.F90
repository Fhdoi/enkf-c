module read_write_nc	

        use netcdf
	
	use Global_variables, only: nx,ny
	contains	

	subroutine read_var_nc(file_name, var_name, nice, ndims, restart_in)
		implicit none
		character (len = *), intent(in) :: file_name
		character (len = *), intent(in) :: var_name
		integer, intent(in) 		:: ndims, nice
		real, intent(out) 		:: restart_in(nx,ny,nice)
		
		integer :: temp_varid
		integer :: start(ndims), count(ndims)
		integer :: ncid
		
		! open the file
		call check( nf90_open(file_name, nf90_nowrite, ncid) )
	
		!get varid
		call check( nf90_inq_varid(ncid, var_name, temp_varid) )
	
		!Defining a vector
		count = (/nx, ny, nice/)
		start = (/ 1, 1, 1/)
		

		!read data
		call check( nf90_get_var(ncid, temp_varid, restart_in, start = start, count = count)) 
	
		call check( nf90_close(ncid) )
	end subroutine read_var_nc
	
	subroutine check(status)
		integer, intent ( in) :: status
    
    		if(status /= nf90_noerr) then 
      			print *, trim(nf90_strerror(status))
      			stop "Stopped"
    		end if
	end subroutine check
	
	subroutine write_var_nc(file_name, var_name, nice, ndims, restart_in)
		character (len = *), intent(in) :: file_name
		character (len = *), intent(in) :: var_name
		integer, intent(in) :: ndims, nice
		real, intent(in) :: restart_in(nx,ny,nice)
		
		integer :: temp_varid
		integer :: start(ndims), count(ndims)
		integer :: ncid
		
		! Burde antageligvis lage en mer generell kode etterhvert
		count = (/ nx, ny, nice/)
  		!count = (/ nlats, nlons, nice/)
		start = (/ 1, 1, 1/)

  		
  		call check( nf90_open(FILE_NAME, nf90_write, ncid) )
	
		!get varid
		call check( nf90_inq_varid(ncid, VAR_NAME, temp_varid) )
		call check( nf90_put_var(ncid, temp_varid, restart_in, start = start, count = count) )
	
		call check( nf90_close(ncid) )
  		
  	end subroutine write_var_nc

	subroutine write_scalar_nc(file_name, var_name, scalar)
		character (len = *), intent(in) :: file_name
		character (len = *), intent(in) :: var_name
		real, intent(in) :: scalar
		
		integer :: temp_varid
		integer :: ncid

  		
  		call check( nf90_open(FILE_NAME, nf90_write, ncid) )
	
		!get varid
		call check( nf90_inq_varid(ncid, VAR_NAME, temp_varid) )
		call check( nf90_put_var(ncid, temp_varid, scalar) )
	
		call check( nf90_close(ncid) )
  		
  	end subroutine write_scalar_nc


	subroutine create_nc(FILE_NAME, NX, NY,NZ, N, NDIMS)
		
		character(len=*), intent(in) 			:: FILE_NAME
		integer, intent(in) 				:: NX, NY, NDIMS, N
		integer 					:: ncid, varid, dimids(NDIMS), dimids2(2), dimids3(NDIMS), dimids4(NDIMS)
  		integer 					:: x_dimid, y_dimid, z_dimid, n_dimid

		call check( nf90_create(FILE_NAME, NF90_CLOBBER, ncid) )
		call check( nf90_def_dim(ncid, "x", NX, x_dimid) )
		call check( nf90_def_dim(ncid, "y", NY, y_dimid) )
		call check( nf90_def_dim(ncid, "z", NZ, z_dimid) )
		call check( nf90_def_dim(ncid, "Ens", N, n_dimid) )
		dimids =  (/ x_dimid, y_dimid, n_dimid /)
		dimids2 =  (/ x_dimid, y_dimid/)
		dimids3 =  (/ x_dimid, y_dimid, z_dimid/)
		dimids4 =  (/ x_dimid, y_dimid, 35/)

		call check( nf90_def_var(ncid, "A_before", NF90_REAL, dimids, varid) )
		call check( nf90_def_var(ncid, "A_after", NF90_REAL, dimids, varid) )
		call check( nf90_def_var(ncid, "V_before", NF90_REAL, dimids, varid) )
		call check( nf90_def_var(ncid, "V_after", NF90_REAL, dimids, varid) )
		call check( nf90_def_var(ncid, "Obs", NF90_REAL, dimids3, varid) )
		call check( nf90_def_var(ncid, "pObs", NF90_REAL, dimids3, varid) )
		call check( nf90_def_var(ncid, "srf", NF90_REAL, dimids2, varid) )

		call check( nf90_def_var(ncid, "A1_before", NF90_REAL, dimids3, varid) )
		call check( nf90_def_var(ncid, "A1_after", NF90_REAL, dimids3, varid) )	

		call check( nf90_def_var(ncid, "sst_before", NF90_REAL, dimids4, varid) )
		call check( nf90_def_var(ncid, "sst_after", NF90_REAL, dimids4, varid) )	
		
		call check( nf90_enddef(ncid) ) 		
		call check( nf90_close(ncid) )

	end subroutine create_nc	

	subroutine create_nc_mean(FILE_NAME, NX, NY, N, NDIMS)
		
		character(len=*), intent(in) 			:: FILE_NAME
		integer, intent(in) 					:: NX, NY, NDIMS, N
		integer 								:: ncid, varid, dimids(1), dimids2(NDIMS)
  		integer 								:: x_dimid, y_dimid, n_dimid, dimid1

		call check( nf90_create(FILE_NAME, NF90_CLOBBER, ncid) )
		call check( nf90_def_dim(ncid, "x", NX, x_dimid) )
		call check( nf90_def_dim(ncid, "y", NY, y_dimid) )
		call check( nf90_def_dim(ncid, "Ens", N, n_dimid) )
		call check( nf90_def_dim(ncid, "R", 1, dimid1) )
		dimids =  (/ dimid1 /)
		dimids2 =  (/ x_dimid, y_dimid/)

		call check( nf90_def_var(ncid, "A_before", NF90_REAL, dimids2, varid) )
		call check( nf90_def_var(ncid, "A_after", NF90_REAL, dimids2, varid) )
		call check( nf90_def_var(ncid, "Obs", NF90_REAL, dimids2, varid) )		
		call check( nf90_def_var(ncid, "RMSE", NF90_REAL, dimids, varid) )			


		call check( nf90_enddef(ncid) ) 		
		call check( nf90_close(ncid) )

	end subroutine create_nc_mean
end module read_write_nc
