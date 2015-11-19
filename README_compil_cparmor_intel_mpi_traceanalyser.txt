
Configuration:
=============
module load  intel-comp intel-mpi

module list
Currently Loaded Modulefiles:
  1) netcdf-intel/3.6.3-11.1.073   2) intel-comp/2015.3.187         3) intel-mpi/5.0.3.048

setenv MPI_INC /appli/intel/impi/5.0.3.048/intel64/include
setenv MPI_LIB /appli/intel/impi/5.0.3.048/intel64/lib

./configure --prefix=/home2/caparmor/grima/3Ddiffusion/IntelMPI --enable-intel_mpi --enable-traceanalyser --enable-netcdf --enable-optimization=aggressive

Configuration Parameters:
------------------------
3Ddiffusion's version........ 0.0.4_MPI
prefix............... /home2/caparmor/grima/3Ddiffusion/IntelMPI
FC................... mpiifort -trace
FCFLAGS.............. -g -O3 -xHost -ipo -I/appli/intel/impi/5.0.3.048/intel64/include -I/export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/include
FCFLAGS_f90.......... 
LDFLAGS.............. -g -O3 -xHost -ipo -I/appli/intel/impi/5.0.3.048/intel64/include -L/appli/intel/impi/5.0.3.048/intel64/lib -lmpi
LIBS.................  -L/export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/lib -lnetcdf
ENABLE_OPTIMIZATION.. aggressive
ENABLE_NETCDF........ yes
  -NETCDF_LIB........ /export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/lib
  -NETCDF_INC........ /export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/include
ENABLE_PROFILING..... no
ENABLE_QUAD.......... no
host................. x86_64-unknown-linux-gnu

Thu Nov 19 10:54:58 GMT 2015


Submission:
=========
Submit:
______
qsub -l proctype=westmere file.pbs (doesn(t work because select is used in pbs file !!!)
qsub file.pbs

PBSfile:
_______

#!/bin/csh
#PBS -N 3DdiffMPI
#PBS -q parallel8
#PBS -l select=1:ncpus=8:mpiprocs=8

set echo

########################################################
##        3Ddiffusion  in MPI mode on CapArmor       ##
########################################################
## Nicolas.Grima@univ-brest.fr ##
pwd
uname -n

############################
# get the path for mpirun ##
############################
source /usr/share/modules/init/csh
module load netcdf-intel/3.6.3-11.1.073
module load intel-comp 
module load intel-mpi

#################################
# get the path for library MKL ##
#################################
setenv MKL_SERIAL YES

#####################################################
###########            ON CAPARMOR        ###########
#####################################################
    cd $PBS_O_WORKDIR
    ls -rtl
    date
    time mpirun -np 8 3Ddiffusion
    date
    ls -rtl


RESULTS:
=======
1x1: 462s
2x2: 172s
4x4:  46s
8x8: 

2x8 : 46s
8x2 : 55s
1x16: 52s
16x1: 86s

64x1024x1024
1x4x4 : 
