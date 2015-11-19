
module load mpt/2.06 intel-comp mpinside

module list
Currently Loaded Modulefiles:
  1) netcdf-intel/3.6.3-11.1.073   3) intel-comp/2015.3.187
  2) mpt/2.06                      4) mpinside/3.5.3

setenv MPI_INC /appli/mpt/2.06/include
setenv MPI_LIB /appli/mpt/2.06/lib

./configure --prefix=/home2/caparmor/grima/3Ddiffusion/MPInside --enable-mpt --enable-netcdf


Configuration Parameters:
------------------------
3Ddiffusion's version........ 0.0.3_MPI
prefix............... /home2/caparmor/grima/3Ddiffusion
FC................... mpif90
FCFLAGS.............. -g -O2 -I/appli/mpt/2.06/include -I/export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/include
FCFLAGS_f90.......... 
LDFLAGS..............  -I/appli/mpt/2.06/include -L/appli/mpt/2.06/lib -lmpi
LIBS.................  -L/export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/lib -lnetcdf
ENABLE_OPTIMIZATION.. normal
ENABLE_NETCDF........ yes
  -NETCDF_LIB........ /export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/lib
  -NETCDF_INC........ /export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/include
ENABLE_PROFILING..... no
ENABLE_QUAD.......... no
host................. x86_64-unknown-linux-gnu

Wed Nov 18 14:36:01 GMT 2015
