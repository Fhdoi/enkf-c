program Prep_assim

	use netcdf
	use mpi

	use read_write_nc, only: read_var_nc, write_var_nc, create_nc

	use Global_variables, only: nx, ny, Ensemble_dir, restart_dir, cat_sum, ens_size

	implicit none

	! Ocean Variables
	real, dimension(nx,ny,35)			:: Temp, Salt

	! Ice Variables
	real, dimension(nx,ny)				:: 	accum_aice, accum_fhocn, accum_fresh, accum_fswthru, accum_strocnx, accum_strocny, &
											accum_fsalt, fsnow,scale_factor, stress12_1,stress12_2, &
											stress12_3, stress12_4, stressm_1, stressm_2, stressm_3, stressm_4, &
											stressp_1, stressp_2, stressp_3, stressp_4, strocnxT, strocnyT, swidf, &
											swidr, swvdf, swvdr, uvel, vvel, iceumask
	real, dimension(nx,ny,5) 			::	Tsfcn, aicen, alvl, apnd, dhs, ffrac, hpnd, iage, ipnd, qice001, &
											qice002, qice003, qice004, qice005, qice006, qice007, qsno001, sice001, &
											sice002, sice003, sice004, sice005, sice006, sice007, vicen, vlvl, vsnon

	real, dimension(nx,ny,5)			:: thickness
	real, dimension(nx,ny)				:: aic
	
	character(len=90)				:: filein_ice, filein_ocn, dir
	character(len=1) 				:: str
	character(len=2) 				:: str2
	character(len=3) 				:: str3
	integer						:: member, i,j,k

	print*, "Prep"

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

	filein_ice = Restart_dir // 'iced_mem' // str3 // '.nc'
	filein_ocn = Restart_dir // 'ocean_mem' // str3 // '.nc'

        !print*, filein_ice
        !print*, filein_ocn
	! Write to restart file
	! Read analysis results 2d ice
	call read_var_nc(filein_ice, 'accum_aice',  1, 2, accum_aice)
	call read_var_nc(filein_ice, 'accum_fhocn',  1, 2, accum_fhocn)
	call read_var_nc(filein_ice, 'accum_fresh',  1, 2, accum_fresh)
	call read_var_nc(filein_ice, 'accum_fsalt',  1, 2, accum_fsalt)
	call read_var_nc(filein_ice, 'accum_fswthru', 1, 2, accum_fswthru)
	call read_var_nc(filein_ice, 'accum_strocnx', 1, 2, accum_strocnx)
	call read_var_nc(filein_ice, 'accum_strocny', 1, 2, accum_strocny)
	call read_var_nc(filein_ice, 'fsnow',  1, 2, fsnow)
	call read_var_nc(filein_ice, 'iceumask',  1, 2, iceumask)
	call read_var_nc(filein_ice, 'scale_factor',  1, 2, scale_factor)
	call read_var_nc(filein_ice, 'stress12_1',  1, 2, stress12_1)
	call read_var_nc(filein_ice, 'stress12_2',  1, 2, stress12_2)
	call read_var_nc(filein_ice, 'stress12_3',  1, 2, stress12_3)
	call read_var_nc(filein_ice, 'stress12_4',  1, 2, stress12_4)
	call read_var_nc(filein_ice, 'stressm_1',  1, 2, stressm_1)
	call read_var_nc(filein_ice, 'stressm_2',  1, 2, stressm_2)
	call read_var_nc(filein_ice, 'stressm_3',  1, 2, stressm_3)
	call read_var_nc(filein_ice, 'stressm_4',  1, 2, stressm_4)
	call read_var_nc(filein_ice, 'stressp_1',  1, 2, stressp_1)
	call read_var_nc(filein_ice, 'stressp_2',  1, 2, stressp_2)
	call read_var_nc(filein_ice, 'stressp_3',  1, 2, stressp_3)
	call read_var_nc(filein_ice, 'stressp_4',  1, 2, stressp_4)
	call read_var_nc(filein_ice, 'strocnxT',  1, 2, strocnxT)
	call read_var_nc(filein_ice, 'strocnyT',  1, 2, strocnyT)
	call read_var_nc(filein_ice, 'swidf',  1, 2, swidf)
	call read_var_nc(filein_ice, 'swidr',  1, 2, swidr)
	call read_var_nc(filein_ice, 'swvdf',  1, 2, swvdf)
	call read_var_nc(filein_ice, 'swvdr',  1, 2, swvdr)
	call read_var_nc(filein_ice, 'uvel',  1, 2, uvel)
	call read_var_nc(filein_ice, 'vvel',  1, 2, vvel)

	! Read analysis results 3d ice
	call read_var_nc(filein_ice, 'Tsfcn',  5, 3, Tsfcn)
	call read_var_nc(filein_ice, 'aicen',  5, 3, aicen)
	call read_var_nc(filein_ice, 'alvl',  5, 3, alvl)
	call read_var_nc(filein_ice, 'apnd',  5, 3, apnd)
	call read_var_nc(filein_ice, 'dhs',  5, 3, dhs)
	call read_var_nc(filein_ice, 'ffrac',  5, 3, ffrac)
	call read_var_nc(filein_ice, 'hpnd',  5, 3, hpnd)
	call read_var_nc(filein_ice, 'iage',  5, 3, iage)
	call read_var_nc(filein_ice, 'ipnd',  5, 3, ipnd)
	call read_var_nc(filein_ice, 'qice001',  5, 3, qice001)
	call read_var_nc(filein_ice, 'qice002',  5, 3, qice002)
	call read_var_nc(filein_ice, 'qice003',  5, 3, qice003)
	call read_var_nc(filein_ice, 'qice004',  5, 3, qice004)
	call read_var_nc(filein_ice, 'qice005',  5, 3, qice005)
	call read_var_nc(filein_ice, 'qice006',  5, 3, qice006)
	call read_var_nc(filein_ice, 'qice007',  5, 3, qice007)
	call read_var_nc(filein_ice, 'qsno001',  5, 3, qsno001)
	call read_var_nc(filein_ice, 'sice001',  5, 3, sice001)
	call read_var_nc(filein_ice, 'sice002',  5, 3, sice002)
	call read_var_nc(filein_ice, 'sice003',  5, 3, sice003)
	call read_var_nc(filein_ice, 'sice004',  5, 3, sice004)
	call read_var_nc(filein_ice, 'sice005',  5, 3, sice005)
	call read_var_nc(filein_ice, 'sice006',  5, 3, sice006)
	call read_var_nc(filein_ice, 'sice007',  5, 3, sice007)
	call read_var_nc(filein_ice, 'vicen',  5, 3, vicen)
	call read_var_nc(filein_ice, 'vlvl',  5, 3, vlvl)
	call read_var_nc(filein_ice, 'vsnon',  5, 3, vsnon)

	! Read analysis results 3d ocn
	call read_var_nc(filein_ocn, 'temp',  35, 3, temp)
	call read_var_nc(filein_ocn, 'salt',  35, 3, salt)

	
	! Calculate thickness
	do i=1,nx
		do j=1,ny
			do k=1,5
				thickness(i,j,k) = vicen(i,j,k)/aicen(i,j,k)
			enddo
		enddo
	enddo
	
	! Create individual files
	!call create_nc(FILE_NAME,VAR_NAME, zdim)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_accum_aice.nc', 'accum_aice',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_accum_fhocn.nc', 'accum_fhocn',  1)

	call create_nc(Ensemble_dir // 'mem' // str3 // '_accum_fresh.nc', 'accum_fresh',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_accum_fsalt.nc', 'accum_fsalt',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_accum_fswthru.nc', 'accum_fswthru',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_accum_strocnx.nc', 'accum_strocnx', 1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_accum_strocny.nc', 'accum_strocny', 1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_iceumask.nc', 'iceumask',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_fsnow.nc', 'fsnow',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_scale_factor.nc', 'scale_factor',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stress12_1.nc', 'stress12_1',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stress12_2.nc', 'stress12_2',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stress12_3.nc', 'stress12_3',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stress12_4.nc', 'stress12_4',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stressm_1.nc', 'stressm_1',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stressm_2.nc', 'stressm_2',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stressm_3.nc', 'stressm_3',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stressm_4.nc', 'stressm_4',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stressp_1.nc', 'stressp_1',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stressp_2.nc', 'stressp_2',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stressp_3.nc', 'stressp_3',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_stressp_4.nc', 'stressp_4',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_strocnxT.nc', 'strocnxT',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_strocnyT.nc', 'strocnyT',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_swidf.nc', 'swidf',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_swidr.nc', 'swidr',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_swvdf.nc', 'swvdf',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_swvdr.nc', 'swvdr',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_uvel.nc', 'uvel',  1)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_vvel.nc', 'vvel',  1)

	! Read analysis results 3d ice
	call create_nc(Ensemble_dir // 'mem' // str3 // '_Tsfcn.nc', 'Tsfcn',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_aicen.nc', 'aicen',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_alvl.nc', 'alvl',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_apnd.nc', 'apnd',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_dhs.nc', 'dhs',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_ffrac.nc', 'ffrac',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_hpnd.nc', 'hpnd',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_iage.nc', 'iage',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_ipnd.nc', 'ipnd',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_qice001.nc', 'qice001',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_qice002.nc', 'qice002',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_qice003.nc', 'qice003',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_qice004.nc', 'qice004',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_qice005.nc', 'qice005',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_qice006.nc', 'qice006',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_qice007.nc', 'qice007',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_qsno001.nc', 'qsno001',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_sice001.nc', 'sice001',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_sice002.nc', 'sice002',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_sice003.nc', 'sice003',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_sice004.nc', 'sice004',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_sice005.nc', 'sice005',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_sice006.nc', 'sice006',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_sice007.nc', 'sice007',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_vicen.nc', 'vicen',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_vlvl.nc', 'vlvl',  5)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_vsnon.nc', 'vsnon',  5)


	! Read analysis results 3d ocn
	call create_nc(Ensemble_dir // 'mem' // str3 // '_temp.nc', 'temp',  35)
	call create_nc(Ensemble_dir // 'mem' // str3 // '_salt.nc', 'salt',  35)

	
	
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_accum_aice.nc', 'accum_aice',  1, 2, accum_aice)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_accum_fhocn.nc', 'accum_fhocn',  1, 2, accum_fhocn)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_accum_fresh.nc', 'accum_fresh',  1, 2, accum_fresh)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_accum_fsalt.nc', 'accum_fsalt',  1, 2, accum_fsalt)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_accum_fswthru.nc', 'accum_fswthru', 1, 2, accum_fswthru)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_accum_strocnx.nc', 'accum_strocnx', 1, 2, accum_strocnx)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_accum_strocny.nc', 'accum_strocny', 1, 2, accum_strocny)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_fsnow.nc', 'fsnow',  1, 2, fsnow)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_iceumask.nc', 'iceumask',  1, 2, iceumask)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_scale_factor.nc', 'scale_factor',  1, 2, scale_factor)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stress12_1.nc', 'stress12_1',  1, 2, stress12_1)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stress12_2.nc', 'stress12_2',  1, 2, stress12_2)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stress12_3.nc', 'stress12_3',  1, 2, stress12_3)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stress12_4.nc', 'stress12_4',  1, 2, stress12_4)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stressm_1.nc', 'stressm_1',  1, 2, stressm_1)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stressm_2.nc', 'stressm_2',  1, 2, stressm_2)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stressm_3.nc', 'stressm_3',  1, 2, stressm_3)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stressm_4.nc', 'stressm_4',  1, 2, stressm_4)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stressp_1.nc', 'stressp_1',  1, 2, stressp_1)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stressp_2.nc', 'stressp_2',  1, 2, stressp_2)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stressp_3.nc', 'stressp_3',  1, 2, stressp_3)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_stressp_4.nc', 'stressp_4',  1, 2, stressp_4)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_strocnxT.nc', 'strocnxT',  1, 2, strocnxT)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_strocnyT.nc', 'strocnyT',  1, 2, strocnyT)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_swidf.nc', 'swidf',  1, 2, swidf)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_swidr.nc', 'swidr',  1, 2, swidr)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_swvdf.nc', 'swvdf',  1, 2, swvdf)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_swvdr.nc', 'swvdr',  1, 2, swvdr)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_uvel.nc', 'uvel',  1, 2, uvel)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_vvel.nc', 'vvel',  1, 2, vvel)

	! Read analysis results 3d ice
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_Tsfcn.nc', 'Tsfcn',  5, 3, Tsfcn)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_aicen.nc', 'aicen',  5, 3, aicen)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_alvl.nc', 'alvl',  5, 3, alvl)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_apnd.nc', 'apnd',  5, 3, apnd)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_dhs.nc', 'dhs',  5, 3, dhs)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_ffrac.nc', 'ffrac',  5, 3, ffrac)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_hpnd.nc', 'hpnd',  5, 3, hpnd)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_iage.nc', 'iage',  5, 3, iage)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_ipnd.nc', 'ipnd',  5, 3, ipnd)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_qice001.nc', 'qice001',  5, 3, qice001)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_qice002.nc', 'qice002',  5, 3, qice002)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_qice003.nc', 'qice003',  5, 3, qice003)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_qice004.nc', 'qice004',  5, 3, qice004)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_qice005.nc', 'qice005',  5, 3, qice005)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_qice006.nc', 'qice006',  5, 3, qice006)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_qice007.nc', 'qice007',  5, 3, qice007)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_qsno001.nc', 'qsno001',  5, 3, qsno001)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_sice001.nc', 'sice001',  5, 3, sice001)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_sice002.nc', 'sice002',  5, 3, sice002)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_sice003.nc', 'sice003',  5, 3, sice003)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_sice004.nc', 'sice004',  5, 3, sice004)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_sice005.nc', 'sice005',  5, 3, sice005)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_sice006.nc', 'sice006',  5, 3, sice006)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_sice007.nc', 'sice007',  5, 3, sice007)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_vicen.nc', 'vicen',  5, 3, vicen)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_vlvl.nc', 'vlvl',  5, 3, vlvl)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_vsnon.nc', 'vsnon',  5, 3, vsnon)



	! Read analysis results 3d ocn
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_temp.nc', 'temp',  35, 3, temp)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_salt.nc', 'salt',  35, 3, salt)


	call cat_sum(aicen, 5, aic)

	! Observed variables
	! Sum of aicen
	call create_nc(Ensemble_dir // 'mem' // str3 // '_aic.nc', 'aic',  1)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_aic.nc', 'aic',  1, 2, aic)

	
	!Thickness
	call create_nc(Ensemble_dir // 'mem' // str3 // '_thickness.nc', 'thickness',  5)
	call write_var_nc(Ensemble_dir // 'mem' // str3 // '_thickness.nc', 'thickness',  1, 2, thickness)

	enddo


end program Prep_assim
