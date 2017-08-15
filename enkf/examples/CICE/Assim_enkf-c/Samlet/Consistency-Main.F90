program Consistency

	use netcdf
	use mpi

	use read_write_nc, only: read_var_nc, write_var_nc
	use Global_variables, only: Analyse_dir, nx, ny, ens_size, Restart_dir, get_date
	use checks, only: aicen_check, vicen_check, Fix_energetics_mp, Fix_energetics_1, cat_sum, &
						create_icemask, generic_check_ocn, generic_check_2d, generic_check_3d

	implicit none



	! Ice Variables
	real, dimension(nx,ny)				:: 	sst,frzmlt,frz_onset, fsnow,scale_factor, stress12_1,stress12_2, &
											stress12_3, stress12_4, stressm_1, stressm_2, stressm_3, stressm_4, &
											stressp_1, stressp_2, stressp_3, stressp_4, strocnxT, strocnyT, swidf, &
											swidr, swvdf, swvdr, uvel, vvel, iceumask
	real, dimension(nx,ny)   			:: 	sst_org, frzmlt_org, frz_onset_org, fsnow_org,scale_factor_org, &
											stress12_1_org,stress12_2_org, stress12_3_org, stress12_4_org, &
											stressm_1_org, stressm_2_org, stressm_3_org, stressm_4_org, &
											stressp_1_org, stressp_2_org, stressp_3_org, stressp_4_org, &
											strocnxT_org, strocnyT_org, swidf_org, swidr_org, swvdf_org, &
											swvdr_org, uvel_org, vvel_org, iceumask_org

	real, dimension(nx,ny,5) 			::	Tsfcn, aicen, alvl, apnd, dhs, ffrac, hpnd, iage, ipnd, qice001, &
											qice002, qice003, qice004, qice005, qice006, qice007, qsno001, sice001, &
											sice002, sice003, sice004, sice005, sice006, sice007, vicen, vlvl, vsnon

	real, dimension(nx,ny,5)			::	Tsfcn_org, alvl_org, apnd_org, dhs_org, ffrac_org, hpnd_org, iage_org, &
							 				ipnd_org, qice001_org, qice002_org, qice003_org, qice004_org, qice005_org, &
											qice006_org, qice007_org, qsno001_org, sice001_org,sice002_org, sice003_org, &
							 				sice004_org, sice005_org, sice006_org, sice007_org, vicen_org, vlvl_org, &
											vsnon_org, aicen_org

	! Ocean Variables
	real, dimension(nx,ny,35)			:: temp, salt, temp_org, salt_org

	real, dimension(nx,ny)				:: aicensum
	real, dimension(nx,ny,2)			:: temp_stress1, temp_stress2
	
	character(len=90)				:: fileout_ice, fileout_ocn

	character(len=1) 				:: str,estr1
	character(len=2) 				:: str2, estr2
	character(len=3) 				:: str3
	character(len=30) 				:: date
	integer						:: member


	call get_date(date)
	do member = 1,ens_size

		if(member < 10) then
			write( str, '(I1)' ) member
			str3 = '00' // str
			fileout_ice = Restart_dir // 'Ens' // str // '/restart/iced.' // trim(date) //'.nc'
		elseif (member > 9 .and. member < 100) then
			write( str2 , '(I2)' ) member
			str3 = '0' // str2
			fileout_ice = Restart_dir // 'Ens' // str2 // '/restart/iced.' // trim(date) //'.nc'
		else
			write( str3 , '(I3)' ) member

		endif


	!fileout_ice = Restart_dir // 'iced_mem' // str3 // '.nc'
	
	print*, Analyse_dir // 'mem' // str3 //'_aicen.nc.analysis'

	! Read analysis results 2d ice
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sst.nc.analysis', 'sst',  1, 2, sst)
	print*, Analyse_dir // 'mem' // str3 //'_aicen.nc.analysis'
        call read_var_nc(Analyse_dir // 'mem' // str3 //'_frzmlt.nc.analysis', 'frzmlt',  1, 2, frzmlt)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_frz_onset.nc.analysis', 'frz_onset',  1, 2, frz_onset)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_fsnow.nc.analysis', 'fsnow',  1, 2, fsnow)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_iceumask.nc.analysis', 'iceumask',  1, 2, iceumask)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_scale_factor.nc.analysis', 'scale_factor',  1, 2, scale_factor)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stress12_1.nc.analysis', 'stress12_1',  1, 2, stress12_1)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stress12_2.nc.analysis', 'stress12_2',  1, 2, stress12_2)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stress12_3.nc.analysis', 'stress12_3',  1, 2, stress12_3)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stress12_4.nc.analysis', 'stress12_4',  1, 2, stress12_4)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressm_1.nc.analysis', 'stressm_1',  1, 2, stressm_1)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressm_2.nc.analysis', 'stressm_2',  1, 2, stressm_2)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressm_3.nc.analysis', 'stressm_3',  1, 2, stressm_3)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressm_4.nc.analysis', 'stressm_4',  1, 2, stressm_4)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressp_1.nc.analysis', 'stressp_1',  1, 2, stressp_1)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressp_2.nc.analysis', 'stressp_2',  1, 2, stressp_2)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressp_3.nc.analysis', 'stressp_3',  1, 2, stressp_3)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressp_4.nc.analysis', 'stressp_4',  1, 2, stressp_4)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_strocnxT.nc.analysis', 'strocnxT',  1, 2, strocnxT)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_strocnyT.nc.analysis', 'strocnyT',  1, 2, strocnyT)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_swidf.nc.analysis', 'swidf',  1, 2, swidf)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_swidr.nc.analysis', 'swidr',  1, 2, swidr)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_swvdf.nc.analysis', 'swvdf',  1, 2, swvdf)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_swvdr.nc.analysis', 'swvdr',  1, 2, swvdr)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_uvel.nc.analysis', 'uvel',  1, 2, uvel)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_vvel.nc.analysis', 'vvel',  1, 2, vvel)

	! Read analysis results 3d ice
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_Tsfcn.nc.analysis', 'Tsfcn',  5, 3, Tsfcn)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_aicen.nc.analysis', 'aicen',  5, 3, aicen)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_alvl.nc.analysis', 'alvl',  5, 3, alvl)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_apnd.nc.analysis', 'apnd',  5, 3, apnd)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_dhs.nc.analysis', 'dhs',  5, 3, dhs)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_ffrac.nc.analysis', 'ffrac',  5, 3, ffrac)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_hpnd.nc.analysis', 'hpnd',  5, 3, hpnd)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_iage.nc.analysis', 'iage',  5, 3, iage)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_ipnd.nc.analysis', 'ipnd',  5, 3, ipnd)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice001.nc.analysis', 'qice001',  5, 3, qice001)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice002.nc.analysis', 'qice002',  5, 3, qice002)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice003.nc.analysis', 'qice003',  5, 3, qice003)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice004.nc.analysis', 'qice004',  5, 3, qice004)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice005.nc.analysis', 'qice005',  5, 3, qice005)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice006.nc.analysis', 'qice006',  5, 3, qice006)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice007.nc.analysis', 'qice007',  5, 3, qice007)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qsno001.nc.analysis', 'qsno001',  5, 3, qsno001)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice001.nc.analysis', 'sice001',  5, 3, sice001)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice002.nc.analysis', 'sice002',  5, 3, sice002)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice003.nc.analysis', 'sice003',  5, 3, sice003)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice004.nc.analysis', 'sice004',  5, 3, sice004)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice005.nc.analysis', 'sice005',  5, 3, sice005)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice006.nc.analysis', 'sice006',  5, 3, sice006)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice007.nc.analysis', 'sice007',  5, 3, sice007)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_vicen.nc.analysis', 'vicen',  5, 3, vicen)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_vlvl.nc.analysis', 'vlvl',  5, 3, vlvl)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_vsnon.nc.analysis', 'vsnon',  5, 3, vsnon)


	! Read original values
	! Read analysis results 2d ice
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sst.nc', 'sst',  1, 2, sst_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_frzmlt.nc', 'frzmlt',  1, 2, frzmlt_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_frz_onset.nc', 'frz_onset',  1, 2, frz_onset_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_fsnow.nc', 'fsnow',  1, 2, fsnow_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_iceumask.nc', 'iceumask',  1, 2, iceumask_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_scale_factor.nc', 'scale_factor',  1, 2, scale_factor_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stress12_1.nc', 'stress12_1',  1, 2, stress12_1_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stress12_2.nc', 'stress12_2',  1, 2, stress12_2_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stress12_3.nc', 'stress12_3',  1, 2, stress12_3_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stress12_4.nc', 'stress12_4',  1, 2, stress12_4_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressm_1.nc', 'stressm_1',  1, 2, stressm_1_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressm_2.nc', 'stressm_2',  1, 2, stressm_2_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressm_3.nc', 'stressm_3',  1, 2, stressm_3_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressm_4.nc', 'stressm_4',  1, 2, stressm_4_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressp_1.nc', 'stressp_1',  1, 2, stressp_1_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressp_2.nc', 'stressp_2',  1, 2, stressp_2_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressp_3.nc', 'stressp_3',  1, 2, stressp_3_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_stressp_4.nc', 'stressp_4',  1, 2, stressp_4_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_strocnxT.nc', 'strocnxT',  1, 2, strocnxT_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_strocnyT.nc', 'strocnyT',  1, 2, strocnyT_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_swidf.nc', 'swidf',  1, 2, swidf_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_swidr.nc', 'swidr',  1, 2, swidr_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_swvdf.nc', 'swvdf',  1, 2, swvdf_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_swvdr.nc', 'swvdr',  1, 2, swvdr_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_uvel.nc', 'uvel',  1, 2, uvel_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_vvel.nc', 'vvel',  1, 2, vvel_org)

	! Read analysis results 3d ice
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_Tsfcn.nc', 'Tsfcn',  5, 3, Tsfcn_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_aicen.nc', 'aicen',  5, 3, aicen_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_alvl.nc', 'alvl',  5, 3, alvl_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_apnd.nc', 'apnd',  5, 3, apnd_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_dhs.nc', 'dhs',  5, 3, dhs_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_ffrac.nc', 'ffrac',  5, 3, ffrac_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_hpnd.nc', 'hpnd',  5, 3, hpnd_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_iage.nc', 'iage',  5, 3, iage_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_ipnd.nc', 'ipnd',  5, 3, ipnd_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice001.nc', 'qice001',  5, 3, qice001_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice002.nc', 'qice002',  5, 3, qice002_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice003.nc', 'qice003',  5, 3, qice003_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice004.nc', 'qice004',  5, 3, qice004_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice005.nc', 'qice005',  5, 3, qice005_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice006.nc', 'qice006',  5, 3, qice006_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qice007.nc', 'qice007',  5, 3, qice007_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_qsno001.nc', 'qsno001',  5, 3, qsno001_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice001.nc', 'sice001',  5, 3, sice001_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice002.nc', 'sice002',  5, 3, sice002_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice003.nc', 'sice003',  5, 3, sice003_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice004.nc', 'sice004',  5, 3, sice004_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice005.nc', 'sice005',  5, 3, sice005_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice006.nc', 'sice006',  5, 3, sice006_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_sice007.nc', 'sice007',  5, 3, sice007_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_vicen.nc', 'vicen',  5, 3, vicen_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_vlvl.nc', 'vlvl',  5, 3, vlvl_org)
	call read_var_nc(Analyse_dir // 'mem' // str3 //'_vsnon.nc', 'vsnon',  5, 3, vsnon_org)

	call aicen_check(aicen,vicen)
	call vicen_check(aicen,vicen)

	
	call cat_sum(aicen, 5, aicensum)
	! Consistency check 2d
	call generic_check_2d('sst',sst,aicensum,sst_org)
	call generic_check_2d('frzmlt',frzmlt,aicensum,frzmlt_org)
	call generic_check_2d('frz_onset',frz_onset,aicensum,frz_onset_org)
	call generic_check_2d('fsnow',fsnow,aicensum,fsnow_org)
	call generic_check_2d('iceumask',iceumask,aicensum,iceumask)
	call generic_check_2d('scale_factor',scale_factor,aicensum,scale_factor_org)
	call generic_check_2d('stress12_1',stress12_1,aicensum,stress12_1_org)
	call generic_check_2d('stress12_2',stress12_2,aicensum,stress12_2_org)
	call generic_check_2d('stress12_3',stress12_3,aicensum,stress12_3_org)
	call generic_check_2d('stress12_4',stress12_4,aicensum,stress12_4_org)
	call generic_check_2d('stressm_1',stressm_1,aicensum,stressm_1_org)
	call generic_check_2d('stressm_2',stressm_2,aicensum,stressm_2_org)
	call generic_check_2d('stressm_3',stressm_3,aicensum,stressm_3_org)
	call generic_check_2d('stressm_4',stressm_4,aicensum,stressm_4_org)
	call generic_check_2d('stressp_1',stressp_1,aicensum,stressp_1_org)
	call generic_check_2d('stressp_2',stressp_2,aicensum,stressp_2_org)
	call generic_check_2d('stressp_3',stressp_3,aicensum,stressp_3_org)
	call generic_check_2d('stressp_4',stressp_4,aicensum,stressp_4_org)
	call generic_check_2d('strocnxT',strocnxT,aicensum,strocnxT_org)
	call generic_check_2d('strocnyT',strocnyT,aicensum,strocnyT_org)
	call generic_check_2d('swidf',swidf,aicensum,swidf_org)
	call generic_check_2d('swidr',swidr,aicensum,swidr_org)
	call generic_check_2d('swvdf',swvdf,aicensum,swvdf_org)
	call generic_check_2d('swvdr',swvdr,aicensum,swvdr_org)
	call generic_check_2d('uvel',uvel,aicensum,uvel_org)
	call generic_check_2d('vvel',vvel,aicensum,vvel_org)



	! Consistency check 3d
	call generic_check_3d('Tsfcn',Tsfcn,aicen,Tsfcn_org)
	call generic_check_3d('alvl',alvl,aicen,alvl_org)
	call generic_check_3d('apnd',apnd,aicen,apnd_org)
	call generic_check_3d('dhs',dhs,aicen,dhs_org)
	call generic_check_3d('ffrac',ffrac,aicen,ffrac_org)
	call generic_check_3d('hpnd',hpnd,aicen,hpnd_org)
	call generic_check_3d('iage',iage,aicen,iage_org)
	call generic_check_3d('ipnd',ipnd,aicen,ipnd_org)
	call generic_check_3d('qice001',qice001,aicen,qice001_org)
	call generic_check_3d('qice002',qice002,aicen,qice002_org)
	call generic_check_3d('qice003',qice003,aicen,qice003_org)
	call generic_check_3d('qice004',qice004,aicen,qice004_org)
	call generic_check_3d('qice005',qice005,aicen,qice005_org)
	call generic_check_3d('qice006',qice006,aicen,qice006_org)
	call generic_check_3d('qice007',qice007,aicen,qice007_org)
	call generic_check_3d('qsno001',qsno001,aicen,qsno001_org)
	call generic_check_3d('sice001',sice001,aicen,sice001_org)
	call generic_check_3d('sice002',sice002,aicen,sice002_org)
	call generic_check_3d('sice003',sice003,aicen,sice003_org)
	call generic_check_3d('sice004',sice004,aicen,sice004_org)
	call generic_check_3d('sice005',sice005,aicen,sice005_org)
	call generic_check_3d('sice006',sice006,aicen,sice006_org)
	call generic_check_3d('sice007',sice007,aicen,sice007_org)
	call generic_check_3d('vlvl',vlvl,aicen,vlvl_org)
	call generic_check_3d('vsnon',vsnon,aicen,vsnon_org)


	! Variables not to be changed during assimilation



	frzmlt = frzmlt_org
	frz_onset = frz_onset_org
	fsnow = fsnow_org
	iceumask = iceumask_org
	scale_factor = scale_factor_org
	swidf = swidf_org
	swidr = swidr_org
	swvdf = swvdf_org
	swvdr = swvdr_org

	iage = iage_org


	! Fix energetics due to rebinning of variables, this is just testing where energy is assumed to be proportional top	
	! to the energetics. stress12_1 is assumed to be proportional to the volume of the two layers it lies between,
	! TEST PHASE!!! Layer one is assumed to be related to vicen 1
	temp_stress1(:,:,1) = vicen(:,:,1)
	temp_stress1(:,:,2) = vicen(:,:,2)
	temp_stress2(:,:,1) = vicen_org(:,:,1)
	temp_stress2(:,:,2) = vicen_org(:,:,2)
	call Fix_energetics_1(stress12_1,temp_stress1,temp_stress2)

	temp_stress1(:,:,1) = vicen(:,:,2)
	temp_stress1(:,:,2) = vicen(:,:,3)
	temp_stress2(:,:,1) = vicen_org(:,:,2)
	temp_stress2(:,:,2) = vicen_org(:,:,3)
	call Fix_energetics_1(stress12_2,temp_stress1,temp_stress2)

	temp_stress1(:,:,1) = vicen(:,:,3)
	temp_stress1(:,:,2) = vicen(:,:,4)
	temp_stress2(:,:,1) = vicen_org(:,:,3)
	temp_stress2(:,:,2) = vicen_org(:,:,4)
	call Fix_energetics_1(stress12_3,temp_stress1,temp_stress2)

	temp_stress1(:,:,1) = vicen(:,:,4)
	temp_stress1(:,:,2) = vicen(:,:,5)
	temp_stress2(:,:,1) = vicen_org(:,:,4)
	temp_stress2(:,:,2) = vicen_org(:,:,5)
	call Fix_energetics_1(stress12_4,temp_stress1,temp_stress2)

	temp_stress1(:,:,1) = vicen(:,:,1)
	temp_stress1(:,:,2) = vicen(:,:,2)
	temp_stress2(:,:,1) = vicen_org(:,:,1)
	temp_stress2(:,:,2) = vicen_org(:,:,2)
	call Fix_energetics_mp(stressm_1,stressp_1,temp_stress1,temp_stress2)

	temp_stress1(:,:,1) = vicen(:,:,2)
	temp_stress1(:,:,2) = vicen(:,:,3)
	temp_stress2(:,:,1) = vicen_org(:,:,2)
	temp_stress2(:,:,2) = vicen_org(:,:,3)
	call Fix_energetics_mp(stressm_2,stressp_2,temp_stress1,temp_stress2)

	temp_stress1(:,:,1) = vicen(:,:,3)
	temp_stress1(:,:,2) = vicen(:,:,4)
	temp_stress2(:,:,1) = vicen_org(:,:,3)
	temp_stress2(:,:,2) = vicen_org(:,:,4)
	call Fix_energetics_mp(stressm_3,stressp_3,temp_stress1,temp_stress2)

	temp_stress1(:,:,1) = vicen(:,:,4)
	temp_stress1(:,:,2) = vicen(:,:,5)
	temp_stress2(:,:,1) = vicen_org(:,:,4)
	temp_stress2(:,:,2) = vicen_org(:,:,5)
	call Fix_energetics_mp(stressm_4,stressp_4,temp_stress1,temp_stress2)


	Call create_icemask(iceumask, aicensum)



	! Write to restart file
	! Read analysis results 2d ice
        print*, fileout_ice
	call write_var_nc(fileout_ice, 'sst',  1, 2, sst)
	call write_var_nc(fileout_ice, 'frzmlt',  1, 2, frzmlt)
	call write_var_nc(fileout_ice, 'frz_onset',  1, 2, frz_onset)
	call write_var_nc(fileout_ice, 'fsnow',  1, 2, fsnow)
	call write_var_nc(fileout_ice, 'iceumask',  1, 2, iceumask)
	call write_var_nc(fileout_ice, 'scale_factor',  1, 2, scale_factor)
	call write_var_nc(fileout_ice, 'stress12_1',  1, 2, stress12_1)
	call write_var_nc(fileout_ice, 'stress12_2',  1, 2, stress12_2)
	call write_var_nc(fileout_ice, 'stress12_3',  1, 2, stress12_3)
	call write_var_nc(fileout_ice, 'stress12_4',  1, 2, stress12_4)
	call write_var_nc(fileout_ice, 'stressm_1',  1, 2, stressm_1)
	call write_var_nc(fileout_ice, 'stressm_2',  1, 2, stressm_2)
	call write_var_nc(fileout_ice, 'stressm_3',  1, 2, stressm_3)
	call write_var_nc(fileout_ice, 'stressm_4',  1, 2, stressm_4)
	call write_var_nc(fileout_ice, 'stressp_1',  1, 2, stressp_1)
	call write_var_nc(fileout_ice, 'stressp_2',  1, 2, stressp_2)
	call write_var_nc(fileout_ice, 'stressp_3',  1, 2, stressp_3)
	call write_var_nc(fileout_ice, 'stressp_4',  1, 2, stressp_4)
	call write_var_nc(fileout_ice, 'strocnxT',  1, 2, strocnxT)
	call write_var_nc(fileout_ice, 'strocnyT',  1, 2, strocnyT)
	call write_var_nc(fileout_ice, 'swidf',  1, 2, swidf)
	call write_var_nc(fileout_ice, 'swidr',  1, 2, swidr)
	call write_var_nc(fileout_ice, 'swvdf',  1, 2, swvdf)
	call write_var_nc(fileout_ice, 'swvdr',  1, 2, swvdr)
	call write_var_nc(fileout_ice, 'uvel',  1, 2, uvel)
	call write_var_nc(fileout_ice, 'vvel',  1, 2, vvel)

	! Read analysis results 3d ice
	call write_var_nc(fileout_ice, 'Tsfcn',  5, 3, Tsfcn)
	call write_var_nc(fileout_ice, 'aicen',  5, 3, aicen)
	call write_var_nc(fileout_ice, 'alvl',  5, 3, alvl)
	call write_var_nc(fileout_ice, 'apnd',  5, 3, apnd)
	call write_var_nc(fileout_ice, 'dhs',  5, 3, dhs)
	call write_var_nc(fileout_ice, 'ffrac',  5, 3, ffrac)
	call write_var_nc(fileout_ice, 'hpnd',  5, 3, hpnd)
	call write_var_nc(fileout_ice, 'iage',  5, 3, iage)
	call write_var_nc(fileout_ice, 'ipnd',  5, 3, ipnd)
	call write_var_nc(fileout_ice, 'qice001',  5, 3, qice001)
	call write_var_nc(fileout_ice, 'qice002',  5, 3, qice002)
	call write_var_nc(fileout_ice, 'qice003',  5, 3, qice003)
	call write_var_nc(fileout_ice, 'qice004',  5, 3, qice004)
	call write_var_nc(fileout_ice, 'qice005',  5, 3, qice005)
	call write_var_nc(fileout_ice, 'qice006',  5, 3, qice006)
	call write_var_nc(fileout_ice, 'qice007',  5, 3, qice007)
	call write_var_nc(fileout_ice, 'qsno001',  5, 3, qsno001)
	call write_var_nc(fileout_ice, 'sice001',  5, 3, sice001)
	call write_var_nc(fileout_ice, 'sice002',  5, 3, sice002)
	call write_var_nc(fileout_ice, 'sice003',  5, 3, sice003)
	call write_var_nc(fileout_ice, 'sice004',  5, 3, sice004)
	call write_var_nc(fileout_ice, 'sice005',  5, 3, sice005)
	call write_var_nc(fileout_ice, 'sice006',  5, 3, sice006)
	call write_var_nc(fileout_ice, 'sice007',  5, 3, sice007)
	call write_var_nc(fileout_ice, 'vicen',  5, 3, vicen)
	call write_var_nc(fileout_ice, 'vlvl',  5, 3, vlvl)
	call write_var_nc(fileout_ice, 'vsnon',  5, 3, vsnon)


	enddo

end program Consistency
