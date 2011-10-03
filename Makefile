BINTARG = fzcal
ALIBTARG = libfzcal.a 
SOLIBTARG = libfzcal.so.0.0.0
SONAME = libfzcal.so.0

LIBDIR = /usr/local/lib
BINDIR = /usr/local/bin
INCDIR = /usr/local/include

#
FC  = /usr/local/bin/gfortran
CC  = /usr/local/bin/gcc
LD  = /usr/local/bin/gfortran
ASM = yasm
AR  = ar

#
CFLAGS = -c -Wall
FFLAGS = -c -Wall -cpp -fbounds-check\
 -fcray-pointer -fbackslash\
 -g -fPIC
AFLAGS = -g stabs
ARFLAGS = rv
LDFLAGS=-L/usr/local/lib
SOFLAGS = -shared -Wl,-soname,$(SONAME)

LD_RUN_PATH=/usr/local/lib
LD_LIBRARY_PATH=/usr/local/lib

.ifdef _NETWALKER_
    FFLAGS += -D_NO_REAL10_ -D_NO_REAL16_ -D_NO_ASM_ -D_USE32_
    OBJ = memioF.o fpioF.o slist.o plist.o zmath.o rpn.o com.o
.else
    USE64 != uname -m | grep -e x86_64 -e amd64 | wc -c
.   if( $(USE64) == 0 )
        FFLAGS += -D_USE32_
        AFLAGS += -f elf32 -m x86 -D_USE32_
.   else
        FFLAGS += -fdefault-integer-8
        AFLAGS += -f elf64 -m amd64
.   endif
    OBJ = memioA.o memioF.o fpioA.o fpioF.o slist.o plist.o zmath.o rpn.o com.o
.endif

LIBOBJ = $(OBJ) fzc.o
BINOBJ = $(OBJ) fzcal.o

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

$(ALIBTARG): $(LIBOBJ)
	$(AR) $(ARFLAGS) $@ $(.ALLSRC)

$(SOLIBTARG): $(LIBOBJ)
	$(LD) $(LDFLAGS) $(SOFLAGS) -o $@ $(.ALLSRC)

fzcal: $(BINOBJ)
	$(FC) $(LDFLAGS) $(.ALLSRC) -o $@

test: test_rpn.o
	gcc -g -Wall -L/usr/local/lib -lfzcal  $(.ALLSRC) -o $@

install: install-lib install-bin

install-lib: $(ALIBTARG) $(SOLIBTARG) 
	cp -pf $(.ALLSRC) $(LIBDIR)
	cp -pf fzc.h $(INCDIR)

install-bin: $(BINTARG)
	cp -pf $(.ALLSRC) $(BINDIR)

clean:
	rm -f *.o *.mod $(BINTARG) $(ALIBTARG) $(SOLIBTARG) test
