clear all

aic = ncread('aic_201707181200.nc','ice_conc');
lon = ncread('aic_201707181200.nc','lon');
lat = ncread('aic_201707181200.nc','lat');
error = ncread('aic_201707181200.nc','total_uncertainty');

[a,b] = size(aic);
aic = reshape(aic, a*b,1);
lon = reshape(lon, a*b,1);
lat = reshape(lat, a*b,1);
error = reshape(error, a*b,1);
age = zeros(a*b,1);

aic = aic./100;
error = error./100;
aic(isnan(aic) == 1) = 0.001;
error(isnan(error) == 1) = 0.001;

lon = lon + 180;

file = 'Windsat_20170617.nc';
delete(file)


var_name = 'aic';
nccreate(file,var_name,'Dimensions',{'nobs',a*b})
ncwrite(file,var_name,aic)  

nccreate(file,'lat','Dimensions',{'nobs',a*b})
ncwrite(file,'lat',lat) 

nccreate(file,'lon','Dimensions',{'nobs',a*b})
ncwrite(file,'lon',lon)  

nccreate(file,'error','Dimensions',{'nobs',a*b})
ncwrite(file,'error',error)

nccreate(file,'age','Dimensions',{'nobs',a*b})
ncwrite(file,'age',age) 
ncwriteatt(file,'age','units','day');

ncwriteatt(file,'/','units','meter')


