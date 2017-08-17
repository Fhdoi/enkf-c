import os
import sys
string = open("../Ens1/restart/ice.restart_file", "rb").read()
#print string
#print string[46:53]
for i in range(0, len(string)):
	#print string[i]
	
	if string[i:i+6] == '/iced.':
		#print "her", i+1
		start_pos = i+1
#print string[start_pos:start_pos + 24]
filed = string[start_pos+4:start_pos + 24]

#print str(sys.argv)
f = sys.argv
print f[1]
if f[1] == '1':
	print "endrer"
	for i in range(1, 10 + 1):
		os.system('mv ../Ens'+str(i)+'/restart/iced' + filed + ' ' + '../Ens'+str(i)+'/restart/this_day.nc')

# Do things!!



if f[1] == '2':
	for i in range(1, 10 + 1):
		os.system('mv ../Ens'+str(i)+'/restart/this_day.nc' + ' ' + '../Ens'+str(i)+'/restart/iced' + filed  )
	os.system('cp aicen_res.nc /home/sfr009/Results/Rundir_enkf-c/aicen_res'+filed)
	os.system('cp enkf_diag.nc /home/sfr009/Results/Rundir_enkf-c/enkf_diag'+filed)

# Copy observation file
if f[1] == '3':
	os.system('cp /global/work/sfr009/OSISAF/osisaf'+filed+' /global/work/sfr009/Rundir_enkf-c/Assim_enkf-c/obs/this_day.nc')

if f[1] == '4':
        os.system('rm /global/work/sfr009/Rundir_enkf-c/Assim_enkf-c/obs/this_day.nc')



