# makefile3 : letʼs use some makefile variables
objects = Global_variables.F90 read_write_nc.F90 Prep_assim.F90
FC = mpif90
Lib = -lnetcdff -llapack -lblas -lnetcdf
Inc =

# Se litt nærmere på hva jeg trenger her
 FFLAGS = -fpic -mcmodel=large -r8 -CB

 
Prep : $(objects)
	$(FC) -o Prep_assim $(objects) $(Inc) $(Lib) $(FFLAGS) 
%.o : %.f90
	$(FC) ${FFLAGS} -c $< $(Inc) $(Lib) $(FFLAGS) 

%.o : %.F90
	$(FC) ${FFLAGS} -c $< $(Inc) $(Lib) $(FFLAGS)

clean:
	rm -f *.o *.mod *.MOD
veryclean: clean
	rm -f LKF *.out *~ *.dat *.uf *.nc
