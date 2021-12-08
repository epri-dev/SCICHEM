Date: 18 November 2021

Developer contact information:  

 1) Douglas Henn  
    Xator  
    3836 Quakerbridge Road
    Suite 102
    Princeton  
    NJ 08619-1006
    douglas.henn@xatorcorp.com  
    
 2) Prakash Karamchandani  
    Ramboll
    7250 Redwood Boulevard
    Suite 105
    Novato, CA 94945
    pkaramchandani@ramboll.com  


**Source Code and Compilers:**  
  1. Fortran and C  
  2. Intel Fortran, gcc  
  
**Third-Party (and/or Open Source) software: None**  
    *For 64 bit systems (Shared libraries for HDF5 1.8.7 and NETCDF  
     static library version 4.1.2 are included with the distribution)  
  
**Computer system requirements to recompile software:**  
  Linux 64 bit system (x68_64) with Intel Fortran compiler for standard build  
  Windows 8 or above with Intel Fortran and C compilers for advanced build  
 
**Steps to recompile software**  
 
  *Regular build on 64-bit Linux system:*  
  1) Unzip the tar file on a Linux system using:  
         $ tar -xvf SCICHEM-3.3-Binary.tar
  2) Change to build directory:  
         $ cd build/linux  
  3) Run build script:  
         $ bash makeall.sh  
  4) The executable and shared library files listed below will be created  
  under the build/linux/ifort directory as well as copied to bin/linux:  
         metsci, tersci, runsci, sciDOSpost, scipp  
          

  *Advanced build on 32-bit or 64-bit Linux/Unix system**:  
  1) Unzip the tar file on a Linux/Unix system using:  
         $ tar -xvf SCICHEM-3.3-Binary.tar
  2) Download the NETCDF library version 4.1.2 or later as source code  
  or static libraries for the system from  
  [NETCDF Homepage](http://www.unidata.ucar.edu/software/netcdf)  
     If source code is being used then the user will have to create static  
     library based on the build instructions from the netcdf website.  
  3) Download the HDF library version 1.8.7 or later as source code or  
     dynamic libraries for the system from [HDF Homepage](http://www.hdfgroup.org)  
     If source code is being used then the user will have to create  
     dynamic libraries based on the build instructions from the HDF website.  
  4) Change to build directory:  
       $ cd build/linux  
  5) Edit the build script to set the NETCDF_LIB_PATH and HDF_LIB_PATH based  
     on the location of the netcdf and HDF5 libraries.  
  6) If the build is created using a non Intel compiler then the user will  
     have to create a configuration file similar to make.ifort for the  
     compiler being used.  
  7) Change the settings for the "Compiler_Version" and "Compiler" in makeall.sh  
     to compiler being used.  
  8) Run build script:  
       $ bash makeall.sh  
  9) The executable and shared library files listed below will be created  
     under the build/linux/ifort directory as well as copied to bin/linux:  
      metsci, tersci, runsci, sciDOSpost, scipp  
      
*The information is provided to the user for convenience.  
However, the code has not been tested extensively with other compilers or systems.  

 *Regular build on 64-bit Windows system**:  

  Note: Only project files for Intel compiler with Visual Studio 2013 are provided  
  for building the SCIPUFFgui executable for Windows.  

  1) Unzip the tar file "SCICHEM-3.3-Binary.tar" on the Windows system using 7-Zip.  
  2) Open the EPRI.sln solution file with Visual Studio 2013.  
  3) Include the NETCDF and HDF library path in the include path for the projects.  
  4) Build the executable files.  
  5) The executable and dynamic library files listed below should be created:  
   metsci.exe, tersci.exe, runsci.exe , SCIPUFFgui.exe, sciDOSpost.exe,  
   scipp.exe, swim.dll, systool.dll, landuse.dll, messages.dll and SCIPtool.dll  

 *Advanced build on 64-bit Windows system**:  
 
  Note: Only project files for Intel compiler with Visual Studio 2013 is provided  
  for building the SCIPUFFgui executable for Windows.  
  
  1) Unzip the tar file "SCICHEM-3.3-Binary.tar" on the Windows system using 7-Zip.  
  2) Download the NETCDF library version 4.1.2 or later as source code or  
  static libraries for the system from  
  [NETCDF Homepage](http://www.unidata.ucar.edu/software/netcdf)  
  If source code is being used then the user will have to create static library  
  based on the build instructions from the netcdf website.  
  3) Download the HDF library version 1.8.7 or later as source code or dynamic  
  libraries for the system from [HDF Homepage](http://www.hdfgroup.org)  
  If source code is being used then the user will have to create dynamic  
  libraries based on the build instructions from the HDF website.  
  4) Open the EPRI.sln solution file with Visual Studio 2013.  
  5) Include the NETCDF and HDF library path in the include path for the projects.  
  6) Build the executable files.  
  7) The executable and dynamic library files listed below should be created:  
   metsci.exe, tersci.exe, runsci.exe , SCIPUFFgui.exe, sciDOSpost.exe,  
   scipp.exe, swim.dll, systool.dll, landuse.dll, messages.dll and SCIPtool.dll  
      
*The information is provided to the user for convenience. However, the code has  
not been tested extensively for other versions of Visual Studio or systems.  


**Passwords required: None**  

**Lines of code: 330,000**  

**Signature(s):**  
  Douglas Henn  
  Prakash Karamchandani  
