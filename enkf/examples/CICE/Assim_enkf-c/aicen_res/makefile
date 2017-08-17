# makefile3 : letʼs use some makefile variables
objects = Global_variables.F90 read_write_nc.F90 aicen_res.F90
FC = mpif90 
Lib = -L/usr/local/lib -lnetcdff #-llapack -lblas
Inc = -I/usr/local/include

# Se litt nærmere på hva jeg trenger her
FFLAGS = -fpic -mcmodel=large -freal-4-real-8 -Waliasing -Wampersand -Wconversion -Wsurprising -Wc-binding-type -Wintrinsics-std -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Wreal-q-constant -fcheck=all #-Wunused  #-Wall

aicen_res : $(objects)
	$(FC) -o Aice_res $(objects) $(Inc) $(Lib) $(FFLAGS) 
%.o : %.f90
	$(FC) ${FFLAGS} -c $< $(Inc) $(Lib) $(FFLAGS) 

%.o : %.F90
	$(FC) ${FFLAGS} -c $< $(Inc) $(Lib) $(FFLAGS)

clean:
	rm -f *.o *.mod *.MOD
veryclean: clean
	rm -f LKF *.out *~ *.dat *.uf *.nc
