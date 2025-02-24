
SHELL=/bin/sh

MODES=theia-impi theia-mvapich2 wcoss-cray jet-mvapich2 wcoss-cray gaea-cray wcoss-ibm jet-impi

.PHONY: $(MODES) all clean bare unknown

unknown:
	@echo ERROR: Please specify a compilation mode: 1>&2
	@echo "  $(MODES)" 1>&2
	@/bin/false

# Build options:

hera:
	make DM_CC=mpiicc DM_FLAGS=-O all

wcoss-ibm:
	module purge ; \
	module load EnvVars/1.0.0 ; \
	module load ics/15.0.3 ; \
	module load lsf ; \
	module load ibmpe ; \
	make DM_CC=mpiicc DM_FLAGS=-O all

wcoss-cray:
	module purge ; \
	module load PrgEnv-intel ; \
	module rm intel ; \
	module load intel/16.3.210 ; \
	module load xt-lsfhpc/9.1.3 ; \
	module load craype-sandybridge ; \
	make DM_CC=cc DM_FLAGS=-static all

theia-impi:
	module purge ; \
	module load intel impi ; \
	module list ; \
	make DM_CC=mpiicc DM_FLAGS=-O all

theia-mvapich2:
	module purge ; \
	module load intel mvapich2 ; \
	module list ; \
	make DM_CC=mpicc DM_FLAGS=-O all

jet-mvapich2:
	module purge ; \
	module load intel mvapich2 ; \
	module list ; \
	make DM_CC=mpicc DM_FLAGS=-O all

jet-impi:
	source /apps/lmod/lmod/init/sh ; \
	module purge ; \
	module load newdefaults intel impi ; \
	module list ; \
	make DM_CC=mpiicc DM_FLAGS=-O all

jet-openmpi:
	module purge ; \
	module load intel openmpi ; \
	module list ; \
	make DM_CC=mpicc DM_FLAGS=-O all

gaea-cray:
	echo "WARNING: I am assuming you have the right modules loaded." 1>&2 ; \
	module list ; \
	make DM_CC=cc DM_FLAGS=-static all

yellowstone:
	echo "WARNING: I am assuming you have the right modules loaded." ; \
	make DM_CC=mpicc DM_FLAGS=-O all

# Lines from here on down should not need to be changed.

SRC =	mpiserial.c

LIBS  = 
EXEC  =  /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec/mpiserial_stochy

all: $(EXEC)

$(EXEC): $(SRC)
	$(DM_CC) $(DM_FLAGS) -o $(@) $(SRC) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.o *~

bare: clean
	rm -f $(EXEC)
