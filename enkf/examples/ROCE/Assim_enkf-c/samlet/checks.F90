module checks


	use Global_variables, only:	nx,ny

	implicit none	
	contains
	
	subroutine aicen_check(aicen,vicen)

	! This function does consistency check of aicen, the sum of aicen after assimilation seems
	! reasonable, but it might contain several categories with negative values. The negative values
	! are removed, but the sum is retained and the remaing values are weigthed with their values.



	real,intent(inout), dimension(nx,ny,5)		::	aicen, vicen

	real										:: sum_aicen, sum_aicen_lt0	
	integer										:: i,j,k

	do i = 1,nx
		do j = 1,ny
			sum_aicen = 0
			do k = 1,5
				sum_aicen = sum_aicen + aicen(i,j,k)
			enddo
			if (sum_aicen > 1) then
				sum_aicen = 1
			endif

			if (sum_aicen > 0) then
				sum_aicen_lt0 = 0
				do k = 1,5
					if (aicen(i,j,k) < 0) then
						aicen(i,j,k) = 0
					elseif (vicen(i,j,k) < 1e-5) then
						aicen(i,j,k) = 0
					else
						sum_aicen_lt0 = sum_aicen_lt0 + aicen(i,j,k)
					endif
				enddo

				if (sum_aicen_lt0 > 0) then
					do k = 1,5
						aicen(i,j,k) = sum_aicen*aicen(i,j,k)/sum_aicen_lt0
						if (aicen(i,j,k) > 2) then
							print*, aicen(i,j,k), sum_aicen, sum_aicen_lt0
						endif
					enddo
				else
					aicen(i,j,:) = 0
				endif

				


			else
				aicen(i,j,:) = 0
			endif
		enddo
	enddo
			
	

	end subroutine aicen_check


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!																													!
!																													!
!																													!
!																													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


	subroutine vicen_check(aicen,vicen)

	! This function does consistency check of vicen, this subroutine makes sure that the correct thickness
	! are binned according to the model, [0, 0.64, 1.39, 2.47, 4.57]



	real,intent(inout), dimension(nx,ny,5)		::	aicen, vicen

	integer										:: i,j,k, update
	real										:: h, Vo, dV, sum_vicen, sum_vicen_lt0
	real										:: h_cat(6)

	h_cat(1) = 0
	h_cat(2) = 0.64
	h_cat(3) = 1.39
	h_cat(4) = 2.47
	h_cat(5) = 4.57
	h_cat(6) = 20

	do i = 1,nx
		do j= 1,ny
			sum_vicen = 0
			do k = 1,5
				sum_vicen = sum_vicen + vicen(i,j,k)
			enddo
			! Use the sum as the actual value, think I will do this for all variables
			if (sum_vicen > 0) then
				sum_vicen_lt0 = 0
				do k = 1,5
					if (vicen(i,j,k) < 1e-5) then
						vicen(i,j,k) = 0
					else
						sum_vicen_lt0 = sum_vicen_lt0 + vicen(i,j,k)
					endif
				enddo

				if (sum_vicen_lt0 > 0) then
					do k = 1,5
						vicen(i,j,k) = sum_vicen*vicen(i,j,k)/sum_vicen_lt0
					enddo
				else
					vicen(i,j,:) = 0
				endif
			else
				vicen(i,j,:) = 0
			endif


			do k = 1,5 
				if (aicen(i,j,k) == 0) then
					vicen(i,j,k) = 0
				endif

				if (vicen(i,j,k) > 0) then
					update = 1
					do while ( update == 1 ) 
						update = 0
						
						h = vicen(i,j,k)/aicen(i,j,k)
		
						! Too thick ice, give volume to next category
						if (h > h_cat(k + 1)) then
							update = 1
							! 0.1 is subtracted so new thicknes is within the category and not on the edge
							Vo = (h_cat(k + 1) - 0.1)*aicen(i,j,k)
							dV = vicen(i,j,k) - Vo
							!print*, h, vicen(i,j,k), aicen(i,j,k), dv, Vo	
							vicen(i,j,k) = vicen(i,j,k) - dV
							! This is a simplification to avoid errors
							if (k == 5) then
								! increase concentration 
								! This seems to only occur for very small concentrations
								if (aicen(i,j,k) < 0.1) then
									! 1m max for low concentartions
									aicen(i,j,k) = vicen(i,j,k)
								else
									! 20m max for high concentration
									aicen(i,j,k) = vicen(i,j,k)/20
								endif

							else
								if (vicen(i,j,k+1) > 0) then
									vicen(i,j,k+1) = vicen(i,j,k) + dV
								else
									vicen(i,j,k+1) = 0
								endif
							endif
							
						! Too thin ice, decrease concentration
						! A simplification, but the volume of ice is still the same so should be ok for now
						elseif (h < h_cat(k)) then
							update = 1
							aicen(i,j,k) = vicen(i,j,k)/(h_cat(k) + 0.1)

						endif
					enddo
				else 
					vicen(i,j,k) = 0
					aicen(i,j,k) = 0
				endif
			enddo
		enddo
	enddo
				
	end subroutine vicen_check


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!																													!
!																													!
!																													!
!																													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



	subroutine generic_check_3d(var_name,var,aicen,org)

	real, intent(in)				:: aicen(nx,ny,5), org(nx,ny,5)
	real, intent(inout)				:: var(nx,ny,5) 
	character(len=*), intent(in)	:: var_name

	! Limiting values
	real							:: top,ice0, bot, sum_var, sum_var_lt0
	integer							:: i,j,k, onesided


	bot  = 0.0
	top  = 1000.0
	ice0 = 0.0
	onesided = 1

	if (var_name == 'FY') then
		top = 1
		bot = 0
	elseif (var_name == 'Tsfcn') then
		bot = -20
		top = 0
		ice0 = -1.8
	elseif (var_name == 'alvl') then
		top = 1.0
		ice0 = 0.0
	elseif (var_name == 'apnd') then
		top = 1.0
		bot = 0
		ice0 = 78
	! Denne trenger litt mer informasjon om jeg skal assimilere den
	elseif (var_name == 'dhs') then
		onesided = 0
	elseif (var_name == 'hpnd') then
		bot = 0
		ice0 = 0
	elseif (var_name == 'ffrac') then
		top = 1.0
		bot = 0.0
	elseif (var_name == 'qice001') then
		bot = -3.6*10**8
		top = 0.0
	elseif (var_name == 'qice002') then
		bot = -3.6*10**8
		top = 0.0
	elseif (var_name == 'qice003') then
		bot = -3.6*10**8
		top = 0.0
	elseif (var_name == 'qice004') then
		bot = -3.6*10**8
		top = 0.0
	elseif (var_name == 'qice005') then
		bot = -3.6*10**8
		top = 0.0
	elseif (var_name == 'qice006') then
		bot = -3.6*10**8
		top = 0.0
	elseif (var_name == 'qice007') then
		bot = -3.6*10**8
		top = 0.0
	elseif (var_name == 'qsno001') then
		bot = -1.4*10**8
		top = 0.0
	elseif (var_name == 'sice001') then
		bot = 0
		top = 31
	elseif (var_name == 'sice002') then
		bot = 0
		top = 31
	elseif (var_name == 'sice003') then
		bot = 0
		top = 31
	elseif (var_name == 'sice004') then
		bot = 0
		top = 31
	elseif (var_name == 'sice005') then
		bot = 0
		top = 31
	elseif (var_name == 'sice006') then
		bot = 0
		top = 31
	elseif (var_name == 'sice007') then
		bot = 0
		top = 31
	elseif (var_name == 'vlvl') then
		top = 1.0
		bot = 0
	elseif (var_name == 'vsnon') then
		top = 0.5
		bot = 0
		ice0 = 0
	elseif (var_name == 'iage') then
		top = 78
		bot = 0
		ice0 = 0
	endif

	do i = 1,nx
		do j= 1,ny
			if (onesided == 1) then
				sum_var = 0
				do k = 1,5
					sum_var = sum_var + var(i,j,k)
				enddo
				! Use the sum as the actual value, think I will do this for all variables
				if (sum_var > 0) then
					sum_var_lt0 = 0
					do k = 1,5
						sum_var_lt0 = sum_var_lt0 + var(i,j,k)
					enddo
				endif

				if (sum_var_lt0 > 0) then
					do k = 1,5
						var(i,j,k) = sum_var*var(i,j,k)/sum_var_lt0
					enddo
				else
					var(i,j,:) = 0
				endif
			endif
			do k = 1,5
				if (aicen(i,j,k) == 0 .and. ice0 /= 78) then
					var(i,j,k) = ice0
				endif
				if (var(i,j,k) > top .and. top /= 78) then
					var(i,j,k) = org(i,j,k)
				endif
				if (var(i,j,k) < bot .and. bot /= 78) then
					var(i,j,k) = org(i,j,k)
				endif

					if (aicen(i,j,k) > 0) then
						if (var_name == 'qice001') then
							if (var(i,j,k) > -1.11*10**8) then
								var(i,j,k) = -1.11*10**8
							endif
						elseif (var_name == 'qice002') then
							if (var(i,j,k) > -1.11*10**8) then
								var(i,j,k) = -1.11*10**8
							endif
						elseif (var_name == 'qice003') then
							if (var(i,j,k) > -1.11*10**8) then
								var(i,j,k) = -1.11*10**8
							endif
						elseif (var_name == 'qice004') then
							if (var(i,j,k) > -1.11*10**8) then
								var(i,j,k) = -1.11*10**8
							endif
						elseif (var_name == 'qice005') then
							if (var(i,j,k) > -1.11*10**8) then
								var(i,j,k) = -1.11*10**8
							endif
						elseif (var_name == 'qice006') then
							if (var(i,j,k) > -1.11*10**8) then
								var(i,j,k) = -1.11*10**8
							endif
						elseif (var_name == 'qice007') then
							if (var(i,j,k) > -1.11*10**8) then
								var(i,j,k) = -1.11*10**8
							endif
						elseif (var_name == 'qsno001') then
							if (var(i,j,k) > -1.11*10**8) then
								var(i,j,k) = -1.11*10**8
							endif
						endif
					endif
					
					if (var_name == 'qice001' .and. var(i,j,k) > -1.11*10**8 .and. var(i,j,k) < 0) then
						var(i,j,k) = -1.11*10**8
					endif
					if (var_name == 'qice002' .and. var(i,j,k) > -1.11*10**8 .and. var(i,j,k) < 0) then
						var(i,j,k) = -1.11*10**8
					endif
					if (var_name == 'qice003' .and. var(i,j,k) > -1.11*10**8 .and. var(i,j,k) < 0) then
						var(i,j,k) = -1.11*10**8
					endif
					if (var_name == 'qice004' .and. var(i,j,k) > -1.11*10**8 .and. var(i,j,k) < 0) then
						var(i,j,k) = -1.11*10**8
					endif
					if (var_name == 'qice005' .and. var(i,j,k) > -1.11*10**8 .and. var(i,j,k) < 0) then
						var(i,j,k) = -1.11*10**8
					endif
					if (var_name == 'qice006' .and. var(i,j,k) > -1.11*10**8 .and. var(i,j,k) < 0) then
						var(i,j,k) = -1.11*10**8
					endif
					if (var_name == 'qice007' .and. var(i,j,k) > -1.11*10**8 .and. var(i,j,k) < 0) then
						var(i,j,k) = -1.11*10**8
					endif
					if (var_name == 'qsno001' .and. var(i,j,k) > -1.11*10**8 .and. var(i,j,k) < 0) then
						var(i,j,k) = -1.11*10**8
					endif
			enddo


		enddo
	enddo
	
	end subroutine generic_check_3d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!																													!
!																													!
!																													!
!																													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	subroutine generic_check_2d(var_name,var,ice,org)

		real, intent(in)				:: ice(nx,ny), Org(nx,ny)
		real, intent(inout)				:: var(nx,ny) 
		character(len=*), intent(in)	:: var_name		

		! Limiting values
		real							:: top,ice0, dist
		integer							:: i,j,check0
                double precision                                        :: bot
		bot  = 0.0
		top  = 1000.0
		ice0 = 0.0
		check0 = 0 
		if (var_name == 'aicen') then
			top = 1.0
		elseif (var_name == 'frzmlt') then
			bot = -1000.0
			ice0 = -1000.0
		elseif (var_name == 'fsnow') then
			bot = -2.0
			top = 10.0
			ice0 = 78
		elseif (var_name == 'scale_factor') then
			ice0 = 78
		elseif (var_name == 'stress12_1') then
			bot = 78
			top = 78
		elseif (var_name == 'stress12_2') then
			bot = 78
			top = 78
		elseif (var_name == 'stress12_3') then
			bot = 78
			top = 78
		elseif (var_name == 'stress12_4') then
			bot = 78
			top = 78
		elseif (var_name == 'stressm_1') then
			bot = 78
			top = 78
		elseif (var_name == 'stressm_2') then
			bot = 78
			top = 78
		elseif (var_name == 'stressm_3') then
			bot = 78
			top = 78
		elseif (var_name == 'stressm_4') then
			bot = 78
			top = 78
		elseif (var_name == 'stressp_1') then
			bot = 78
			top = 0
		elseif (var_name == 'stressp_2') then
			bot = 78
			top = 0
		elseif (var_name == 'stressp_3') then
			bot = 78
			top = 0
		elseif (var_name == 'stressp_4') then
			bot = 78
			top = 0
		elseif (var_name == 'strocnxT') then
			bot = 78
			top = 78
		elseif (var_name == 'strocnyT') then
			bot = 78
			top = 78
		elseif (var_name == 'uvel') then
			bot = 78
			top = 78
		elseif (var_name == 'vvel') then
			bot = 78
			top = 78
		endif
		do i=1,nx
			do j=1,ny
				! Check is limits are exceeded and fix
				if (top /= 78) then
					if (var(i,j) > top) then
						var(i,j) = Org(i,j)
					endif
				endif
				if (bot /= 78) then
					if (var(i,j) < bot) then
						var(i,j) = Org(i,j)
					endif
				endif
					
				! No ice
				if (ice(i,j) == 0 .and. ice0 /= 78) then
					var(i,j) = ice0				
				endif
			 
			enddo
		enddo

	end subroutine generic_check_2d


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!																													!
!																													!
!																													!
!																													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	subroutine generic_check_ocn(var_name,var,org)
		
		real, intent(in)				:: Org(nx,ny,35)
		real, intent(inout)				:: var(nx,ny,35) 
		character(len=*), intent(in)	:: var_name		

		! Limiting values
		real							:: top,dist, bot
		integer							:: i,j,k,check0

		if (var_name == 'temp') then
			top = 42.0
			bot = -2.0
		elseif (var_name == 'salt') then
			bot = 28
			top = 39
		endif
		do i=1,nx
			do j=1,ny
				do k=1,35
					! Check is limits are exceeded and fix
					if (top /= 78) then
						if (var(i,j,k) > top) then
							var(i,j,k) = Org(i,j,k)
						endif
					endif
					if (bot /= 78) then
						if (var(i,j,k) < bot) then
							var(i,j,k) = Org(i,j,k)
						endif
					endif
				enddo			 
			enddo
		enddo

	end subroutine generic_check_ocn


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!																													!
!																													!
!																													!
!																													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



	subroutine create_icemask(mask, icesum)
	
	real, intent(in)		:: icesum(nx,ny)
	real, intent(out)		:: mask(nx,ny)

	integer					:: i,j

	do i = 1,nx
		do j = 1,ny
			if (icesum(i,j) > 0) then
				mask(i,j) = 1
			else
				mask(i,j) = 0
			endif
				
		enddo
	enddo
	


	end subroutine create_icemask

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!																													!
!																													!
!																													!
!																													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



	subroutine cat_sum(Var, zdim, Varsum)

	integer, intent(in)		:: zdim	
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



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!		This subroutine is highly experimental and made as a possible way to conserve energetics when rebinning		!
!		ice categories  																							!				
!																													!
!																													!
!																													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



	subroutine Fix_energetics_1(stress,vic,vic_org)
	
	real, intent(in)		:: vic(nx,ny,2), vic_org(nx,ny,2)
	real, intent(inout)		:: stress(nx,ny)

	integer					:: i,j,k
	real					:: dv1, dv2

	do i = 1,nx
		do j = 1,ny
			if (vic_org(i,j,1) > 0 .and. vic_org(i,j,2) > 0) then
				dv1 = (vic(i,j,1) - vic_org(nx,ny,1))/vic_org(nx,ny,1)
				dv2 = (vic(i,j,2) - vic_org(nx,ny,2))/vic_org(nx,ny,2)
				stress(i,j) = stress(i,j)*(1 + dv1 + dv2)
			endif
		enddo
	enddo
	


	end subroutine Fix_energetics_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!		This subroutine is highly experimental and made as a possible way to conserve energetics when rebinning		!
!		ice categories  																							!				
!																													!
!																													!
!																													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



	subroutine Fix_energetics_mp(stressm,stressp,vic,vic_org)
	
	real, intent(in)		:: vic(nx,ny,2), vic_org(nx,ny,2)
	real, intent(inout)		:: stressm(nx,ny), stressp(nx,ny)

	integer					:: i,j,k
	real					:: dv1, dv2,s11,s22

	do i = 1,nx
		do j = 1,ny
			if (vic_org(i,j,1) > 0 .and. vic_org(i,j,2) > 0) then
				s11 = (stressp(i,j) + stressm(i,j))/2
				s22 = (stressp(i,j) - stressm(i,j))/2
				dv1 = (vic(i,j,1) - vic_org(nx,ny,1))/vic_org(nx,ny,1)
				dv2 = (vic(i,j,2) - vic_org(nx,ny,2))/vic_org(nx,ny,2)
				s11 = s11*(1 + dv1 + dv2)
				s22 = s22*(1 + dv1 + dv2)
				stressm(i,j) = s11 - s22
				stressp(i,j) = s11 + s22
			endif
		enddo
	enddo
	


	end subroutine Fix_energetics_mp
	

end module checks
