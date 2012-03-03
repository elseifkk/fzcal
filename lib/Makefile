VERSION   = 0.0.3
SOVERSION = 3

#
BINTARG   = fzcalc
ALIBTARG  = libfzcal.a 
SOLIBTARG = libfzcal.so.$(SOVERSION)
SONAME    = libfzcal.so.$(SOVERSION)
SOFILE    = libfzcal.so

#
LIBDIR = /usr/local/lib
BINDIR = /usr/local/bin
INCDIR = /usr/local/include

#
FC  = /usr/local/bin/gfortran46
CC  = /usr/local/bin/gcc46
LD  = /usr/local/bin/gfortran46
ASM = yasm
AR  = ar

#
CFLAGS = -c -Wall
FFLAGS = -c -Wall -cpp -fbounds-check \
 -fcray-pointer -fbackslash -Wno-conversion \
 -g -fPIC -D_VERSION_=\"$(VERSION)\"
AFLAGS  = -g stabs
ARFLAGS = rv
LDFLAGS = -L/usr/local/lib -L/usr/local/lib/gcc46
SOFLAGS = -shared -Wl,-soname,$(SONAME)

LD_RUN_PATH     = /usr/local/lib:/usr/local/lib/gcc46
LD_LIBRARY_PATH = /usr/local/lib:/usr/local/lib/gcc46

#
.ifdef _NETWALKER_
    FFLAGS += -D_NO_REAL10_ -D_NO_REAL16_ -D_NO_ASM_ -D_USE32_
    OBJ = memioF.o fpioF.o misc.o slist.o plist.o integral.o zmath.o rpng.o rpnt.o rpnlist.o rpnd.o rpnp.o rpne.o com.o
    SOLIBOBJ = $(OBJ)
.else
    USE64 != uname -m | grep -e x86_64 -e amd64 | wc -c
.   if( $(USE64) == 0 )
        FFLAGS += -D_USE32_
        AFLAGS += -f elf32 -m x86 -D_USE32_
.   else
        FFLAGS += -fdefault-integer-8
        AFLAGS += -f elf64 -m amd64
.   endif
    OBJ      = memioA.o   memioF.o fpioF.o misc.o slist.o plist.o integral.o zmath.o rpng.o rpnt.o rpnlist.o rpnd.o rpnp.o rpne.o com.o
    SOLIBOBJ = memioA-d.o memioF.o fpioF.o misc.o slist.o plist.o integral.o zmath.o rpng.o rpnt.o rpnlist.o rpnd.o rpnp.o rpne.o com.o
.endif

#
ALIBOBJ  = $(OBJ) fzc.o
SOLIBOBJ += fzc.o
BINOBJ   = $(OBJ) fzcalc.o

#
.PHONY: clean all install install-lib install-bin

.SUFFIXES:
.SUFFIXES: .o .s .f90 .mod .c

 all: $(ALIBTARG) $(SOLIBTARG) $(BINTARG)

.s.o:
	$(ASM) $(AFLAGS) $<

.c.o:
	$(CC) $(CFLAGS) $<

.f90.o:
	$(FC) $(FFLAGS) $<

.f90.mod:
	$(FC) $(FFLAGS) $<

memioA-d.o: memioA.s
	$(ASM) $(AFLAGS) -D_DYNAMIC_ $(.ALLSRC) -o $@

$(ALIBTARG): $(ALIBOBJ)
	$(AR) $(ARFLAGS) $@ $(.ALLSRC)

$(SOLIBTARG): $(SOLIBOBJ)
	$(LD) $(LDFLAGS) $(SOFLAGS) -o $@ $(.ALLSRC)

$(BINTARG): $(BINOBJ)
	$(FC) $(LDFLAGS) $(.ALLSRC) -o $@

install: install-lib install-bin

install-lib: $(ALIBTARG) $(SOLIBTARG) 
	cp -pRf $(.ALLSRC) $(LIBDIR) 
	cp -pRf fzc.h $(INCDIR)
	ln -sf $(SOLIBTARG) $(SOFILE)
	cp -pRf $(SOFILE) $(LIBDIR)

install-bin: $(BINTARG)
	cp -pRf $(.ALLSRC) $(BINDIR)

clean:
	rm -f *.o *.mod $(BINTARG) $(ALIBTARG) $(SOLIBTARG) $(SOFILE)
