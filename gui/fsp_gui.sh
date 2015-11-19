#!/bin/bash
# fsp_gui.sh
#################################################
# Nicolas.grima@univ-brest.fr  (January - 2013) #
#################################################
#
# Help you to customize your fsp
#
#################################################
#+ Mode normal
ResetColor="$(tput sgr0)"
# "Surligné" (bold)
bold=$(tput smso)
# "Non-Surligné" (offbold)
offbold=$(tput rmso)

# Couleurs (gras)
#+ Rouge
Red="$(tput bold ; tput setaf 1)"
#+ Vert
Green="$(tput bold ; tput setaf 2)"
#+ Jaune
Yellow="$(tput bold ; tput setaf 3)"
#+ Bleue
Blue="$(tput bold ; tput setaf 4)"
#+ Cyan
BlueCyan="$(tput bold ; tput setaf 6)"

IFS=:

applicationName=`zenity --entry --text='Application name' --entry-text='fsp'`

applicationVersion='0.0.0'

authorMail=`zenity --entry --text='Author Mail' --entry-text='author@mail.org'`

applicationURL=`zenity --entry --text='Application URL' --entry-text='http://www.fsp.org'`

applicationInstall=`pwd`
applicationInstall=${applicationInstall%\/*\/*}
applicationInstall=`zenity --entry --text='Where to install the application (--prefix)'\
                           --entry-text=${applicationInstall}`

zenity --question \
       --text='<b><span color=\"red\">Do you want to use NetCDF format in your application</span></b> (--enable-netcdf)'

if [ $? = 0 ]
then
  netcdf='yes'
  arglist='libnetcdf.a'
  netcdfDir=''

  while [ "${netcdfDir}" == '' ]
  do 
    results='Enter here a NetCDF library directory if necessary'
    for arg in ${arglist}
    do
#      echo
#      echo "Searching ${arg} and found it in:"

      for lpath in ${LD_LIBRARY_PATH}
      do
        if [ -e ${lpath}/${arg} ]
        then
#            echo "  - ${lpath}"
            results="${lpath}:${results}"
        fi
      done
    done

    if [ "${results}" == '' ]
    then
      echo ""
      echo "Error: neither NetCDF library are available on this machine !"
      echo "Error: Please install a NetCDF library, using the same Fortran compiler"
      echo "Error: that you will use to compile your application"
      echo "Error: The main Fortran Compilers :"
      echo "Error:     - gfortran:" `which gfortran`
      echo "Error:     - ifort   :" `which ifort`
      echo ""
      exit 1
    fi

    netcdfDir=`zenity --list \
      --text='<b><span color=\"red\">Choose a directory where the NetCDF llibrary is present</span></b>' \
      --editable --column="directory" ${results}`

    if [ $? = 1 ]
    then
      netcdf='no'
    else
      if [ "${netcdfDir}" == 'Enter here a NetCDF library directory if necessary' ]
      then
        netcdfDir=''
      fi
      if [ "${netcdfDir}" == '' ]
      then
	echo ''
        echo " WARNING: please select a valid NetCDF directory or cancel !"
	echo ''
      fi
    fi

    if [ ${netcdf} = 'no' ]
    then
      break
    fi

  done
else
  netcdf='no'
fi

# Remove "lib" at the end of the directory
netcdfDir=${netcdfDir%\/*}


clear

echo "**********************************************"
echo "*    ${Red}Fortran Skeleton Package Gui results${ResetColor}    *"
echo "**********************************************"
echo "Application Name   : ${Green}${applicationName}${ResetColor}"
echo "Application Version: ${Green}${applicationVersion}${ResetColor}"
echo "Author Mail        : ${Green}${authorMail}${ResetColor}"
echo "Application URL    : ${Green}${applicationURL}${ResetColor}"
echo "--prefix           : ${Green}${applicationInstall}${ResetColor}"
if [ ${netcdf} = 'yes' ]
then
  echo "--enable-netcdf    : ${Green}yes${ResetColor}"
  echo "  - NETCDF_LIB     : ${Green}${netcdfDir}/lib${ResetColor}"
  echo "  - NETCDF_INC     : ${Green}${netcdfDir}/include${ResetColor}"
else
  echo "--enable-netcdf    : ${Green}no${ResetColor}"
fi
echo "**********************************************"

echo "**********************************************" > fsp_gui.log
echo "*    Fortran Skeleton Package Gui results    *" >> fsp_gui.log
echo "**********************************************" >> fsp_gui.log
echo "Application Name   : ${applicationName}"        >> fsp_gui.log
echo "Application Version: ${applicationVersion}"     >> fsp_gui.log
echo "Author Mail        : ${authorMail}"             >> fsp_gui.log
echo "Application URL    : ${applicationURL}"         >> fsp_gui.log
echo "--prefix           : ${applicationInstall}"     >> fsp_gui.log
if [ ${netcdf} = 'yes' ]
then
  echo "--enable-netcdf    : yes"                     >> fsp_gui.log
  echo "  - NETCDF_LIB     : ${netcdfDir}/lib"        >> fsp_gui.log
  echo "  - NETCDF_INC     : ${netcdfDir}/include"    >> fsp_gui.log
else
  echo "--enable-netcdf    : no"                      >> fsp_gui.log
fi
echo "**********************************************" >> fsp_gui.log

cd ..

\mv configure.ac configure.ac_old

echo ""
echo "Creating the configure.ac file:"
echo ""                                               >> gui/fsp_gui.log
echo "Creating the configure.ac file:"                >> gui/fsp_gui.log

sed -e "s|http://www.fsp.org|${applicationURL}|g" \
    -e "s/0.0.0/${applicationVersion}/g"          \
    -e "s/author@mail/${authorMail}/g"            \
    -e "s/fsp/${applicationName}/g"               \
    -e "s|\.\.\/\.\.|${applicationInstall}|g"     \
    <configure.ac_orig > configure.ac

cd ./src

echo ""
echo "Creating the prg_${applicationName}.f90 file:"
echo ""                                               >> ../gui/fsp_gui.log
echo "Creating the prg_${applicationName}.f90 file:"  >> ../gui/fsp_gui.log
if [ ! -e "prg_${applicationName}.f90" ]
then
    \ln -s prg_fsp.f90 prg_${applicationName}.f90
fi

cd options

echo ""
echo "Creating Makefile.am files:"
echo ""                                               >> ../../gui/fsp_gui.log
echo "Creating Makefile.am files:"                    >> ../../gui/fsp_gui.log

sed "s/fsp/${applicationName}/g" Makefile.am_fundamental_orig > Makefile.am_fundamental
sed "s/fsp/${applicationName}/g" Makefile.am_netcdf_orig > Makefile.am_netcdf

cd ../..


echo ""
echo "Creating configure file:"
echo ""                                               >> gui/fsp_gui.log
echo "Creating  configure file:"                      >> gui/fsp_gui.log
aclocal; automake; autoconf;

zenity --question \
       --text="Please read the <b><span color=\"red\">FSP parameters</span></b> \
 on your <b><span color=\"red\">terminal</span></b>                         \n\n\
 And answer if you want to  <b><span color=\"red\">configure, compile and\n\
 install</span></b> your application <b><span color=\"red\">${applicationName}</span></b>\
 in the directory \n\
 <b><span color=\"red\">${applicationInstall}</span></b>?"

if [ $? = 0 ]
then

    if [ ${netcdf} = 'yes' ]
    then
      echo ""
      echo "setenv NETCDF_LIB ${netcdfDir}/lib"
      export NETCDF_LIB=${netcdfDir}/lib
      echo "setenv NETCDF_INC  ${netcdfDir}/include"
      export NETCDF_INC=${netcdfDir}/include
      echo ""
      echo "./configure --enable-netcdf"
      ./configure --enable-netcdf                    >> gui/fsp_gui.log
    else
      echo ""
      echo "./configure"
      ./configure                                    >> gui/fsp_gui.log
    fi

    echo "make clean"
    make clean                                       >> gui/fsp_gui.log
    echo "make install"
    make install                                     >> gui/fsp_gui.log

    echo ""
    echo "**************************************************************"
    echo "* Your application ${applicationName} should be installed     "
    echo "* in the directory  ${Green}${applicationInstall}/bin${ResetColor}"
    echo "* You can move to this directory and sumbit  ${Green}./${applicationName}${ResetColor}"
    echo "* or make a symbolink to it, or enter ${applicationInstall}/bin"
    echo "* in you PATH environment variable.                          "
    echo "**************************************************************"


    echo ""                                                                 >> gui/fsp_gui.log
    echo "**************************************************************"   >> gui/fsp_gui.log
    echo "* Your application ${applicationName} should be installed "       >> gui/fsp_gui.log
    echo "* in the directory ${applicationInstall}/bin.                 "   >> gui/fsp_gui.log
    echo "* You can go to this directory and sumbit ./${applicationName} " >> gui/fsp_gui.log
    echo "* or make a symbolink to it, or enter ${applicationInstall}/bin"      >> gui/fsp_gui.log
    echo "* in you PATH environment variable."                              >> gui/fsp_gui.log
    echo "**************************************************************"   >> gui/fsp_gui.log

else

    echo ""
    echo "**************************************************************"
    echo 'Change directory where configure file is (cd ..).'
    echo "Submit the ./configure command using the options if necessary."
    echo "To have more information about ./configure options, enter ./configure --help."
    echo "Two important options are: --prefix and --enable-netcdf."
    echo "If you want to use NetCDF verify that libnetcdf.a is available in /usr/lib."
    echo "If not, set the environemment variable NETCDF_LIB and NETCDF_INC."
    echo "Good Luck :-)"
    echo "**************************************************************"
    echo ""

    echo ""                                                                  >> gui/fsp_gui.log
    echo "**************************************************************"    >> gui/fsp_gui.log
    echo 'Change directory where configure file is (cd ..).'                 >> gui/fsp_gui.log
    echo "Submit the ./configure command using the options if necessary."    >> gui/fsp_gui.log
    echo "To have more information about ./configure options, enter ./configure --help.">> gui/fsp_gui.log
    echo "Two important options are: --prefix and --enable-netcdf."          >> gui/fsp_gui.log
    echo "If you want to use NetCDF verify that libnetcdf.a is available in /usr/lib."  >> gui/fsp_gui.log
    echo "If not, set the environemment variable NETCDF_LIB and NETCDF_INC." >> gui/fsp_gui.log
    echo "Good Luck :-)"                                                     >> gui/fsp_gui.log
    echo "**************************************************************"    >> gui/fsp_gui.log
    echo ""

fi

    echo ""
    echo "Please check the file sfp_gui.log for more information..."
    echo ""

exit 0

# Don't remove this line #