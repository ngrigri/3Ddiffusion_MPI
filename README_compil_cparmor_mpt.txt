
Configuration:
=============
module load mpt/2.06 intel-comp

module list
Currently Loaded Modulefiles:
  1) netcdf-intel/3.6.3-11.1.073   3) intel-comp/2015.3.187
  2) mpt/2.06

setenv MPI_INC /appli/mpt/2.06/include
setenv MPI_LIB /appli/mpt/2.06/lib

./configure --prefix=/home2/caparmor/grima/3Ddiffusion/MPT --enable-mpt --enable-netcdf --enable-optimization=aggressive


Configuration Parameters:
------------------------
3Ddiffusion's version........ 0.0.4_MPI
prefix............... /home2/caparmor/grima/3Ddiffusion/MPT
FC................... mpif90
FCFLAGS.............. -g -O3 -xHost -ipo -I/appli/mpt/2.06/include -I/export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/include
FCFLAGS_f90.......... 
LDFLAGS.............. -g -O3 -xHost -ipo -I/appli/mpt/2.06/include -L/appli/mpt/2.06/lib -lmpi
LIBS.................  -L/export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/lib -lnetcdf
ENABLE_OPTIMIZATION.. aggressive
ENABLE_NETCDF........ yes
  -NETCDF_LIB........ /export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/lib
  -NETCDF_INC........ /export/home/services/bibli/netcdf-3.6.3-intel-11.1.073/include
ENABLE_PROFILING..... no
ENABLE_QUAD.......... no
host................. x86_64-unknown-linux-gnu

Thu Nov 19 10:11:51 GMT 2015


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
module load mpt/2.06 
module load intel-comp

#################################
# get the path for library MKL ##
#################################
setenv MKL_SERIAL YES

#####################################################
###########            ON CAPARMOR        ###########
#####################################################
    cd /work/grima/3Ddiffusion/Test_mpinside_v0
    ls -rtl
    date
    mpiexec_mpt -np 8 3Ddiffusion
    date
    ls -rtl
