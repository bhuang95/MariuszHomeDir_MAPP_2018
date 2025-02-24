# slint Makefile

FLAGS       = -O2 -fp-model precise
SLINTLIB    = ./libslint.a
MKDEPENDS   = ./mkDepends
FC          = ifort

.SUFFIXES:
.SUFFIXES: .F90 .o

.F90.o:
	$(FC) $(FLAGS) -c $<

KDOBJ = kd.o
SLINTOBJ = slint.o uvxyz.o

all: $(SLINTLIB) 

$(SLINTLIB): DEPENDENCIES $(KDOBJ) $(SLINTOBJ) 
	$(AR) ruv $(SLINTLIB) $(SLINTOBJ) $(KDOBJ)

DEPENDENCIES:
	$(RM) -f Filepath Srcfiles
	echo "." > Filepath
	ls -1 *.F90 > Srcfiles
	$(MKDEPENDS) -m Filepath Srcfiles > $@

-include DEPENDENCIES

clean:
	$(RM) *.o *.mod *_cpp.F90 DEPENDENCIES Filepath Srcfiles

# Ning Wang, Feb 2006
# rewritten MP, 2016