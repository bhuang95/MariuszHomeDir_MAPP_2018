# Settings for LIBRARY BUILD ONLY: theia.intel
#
# Flags common to all
RM         = rm -f
AR         = ar
ARFLAGS    =
FC         = mpiifort
FCserial   = ifort
CC         = icc

# Number of parallel tasks for gmake
GMAKEMINUSJ = -j24

# Flags for bacio library
BACIO_FFLAGS  = -O3 -xHOST -traceback
BACIO_CFLAGS  = -O3 -DUNDERSCORE -DLINUX

# Flags for gfsio library
GFSIO_FFLAGS  = -traceback -g -xHOST -convert big_endian -assume byterecl  -I$(INCMOD) -FR
GFSIO_ARFLAGS = -rv

# Flags for ip library
IP_FFLAGS     = -O3 -ip -fp-model strict -real-size 64 -integer-size 32 -qopenmp
IP_ARFLAGS    = -ruv

# Flags for landsfcutil library
LAND_FFLAGS   = -O3 -I$(MOD_DIR) -ip -fp-model strict -real-size 64 -integer-size 32 -FR
LAND_ARFLAGS  = -rv

# Flags for nemsio library
NEMSIO_FFLAGS  = -O -g
NEMSIO_ARFLAGS = -rvu

# Flags for sigio library
SIGIO_FFLAGS  = -O0 -g -xHOST -traceback -free -convert big_endian -assume byterecl -c
SIGIO_ARFLAGS = crvs

# Flags for sp library
SP_FFLAGS  = -O3 -auto -qopenmp -i4 -r8 -convert big_endian -assume byterecl -fp-model strict -fpp -DLINUX
SP_ARFLAGS = -ruv

# Flags for w3emc library
W3EMC_FFLAGS = -O2 -g -traceback -fixed -c
W3EMC_ARFLAGS = ruv

# Flags for w3nco library
W3NCO_FFLAGS  = -O3 -g -r8
W3NCO_CFLAGS  = -O3 -DLINUX
W3NCO_ARFLAGS = -ruv
