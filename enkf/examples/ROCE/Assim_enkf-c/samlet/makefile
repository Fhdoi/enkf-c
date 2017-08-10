# makefile3 : letʼs use some makefile variables
objects = Global_variables.F90 read_write_nc.F90 Prep_assim.f90
objects2 = Global_variables.F90 checks.F90 read_write_nc.F90 Consistency-Main.f90
objects3 = Global_variables.F90 read_write_nc.F90 aicen_res.F90
FC = mpif90 
Lib = -L/usr/local/lib -lnetcdff -llapack -lblas
Inc = -I/usr/local/include

# Se litt nærmere på hva jeg trenger her
FFLAGS = -fpic -mcmodel=large -freal-4-real-8 -Waliasing -Wampersand -Wconversion -Wsurprising -Wc-binding-type -Wintrinsics-std -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Wreal-q-constant -fcheck=all #-Wunused  #-Wall

Prep : $(objects)
	$(FC) -o Prep_assim $(objects) $(Inc) $(Lib) $(FFLAGS)
Check : $(objects2)
	$(FC) -o Check $(objects2) $(Inc) $(Lib) $(FFLAGS)
Aicen : $(objects3)
	$(FC) -o Aicen_res $(objects3) $(Inc) $(Lib) $(FFLAGS)
%.o : %.f90
	$(FC) ${FFLAGS} -c $< $(Inc) $(Lib) $(FFLAGS) 
%.o : %.F90
	$(FC) ${FFLAGS} -c $< $(Inc) $(Lib) $(FFLAGS)

clean:
	rm -f *.o *.mod *.MOD
veryclean: clean
	rm -f Check Prep_assim Aice_res *.out *~ *.dat *.uf *.nc
