# Really neat mostly automatic makefile for C++ projects. By tweaking
# the variables specified on top you should be able to control the
# whole make process. Generates automatic dependency files, allows
# combining of sources from various directories. Only works on GNU-ish
# systems (g++, gnu make, and some generic unix tools are necessary).
#
# Author: Marijn Haverbeke

TARGET = uscheme
PREFIX = /usr/local
TYPE = release
INITFILE = uscheme-init.scm
LIBFILE = libuscheme.a

DIRS = .
LIBS = 
DLIBS =

MACROS = PREFIX=\"$(PREFIX)\" INIT_FILE=\"$(INITFILE)\" # WITH_DESTRUCTORS

ifeq ($(TYPE),debug)
LDPARAM = 
CCPARAM = -Wall -g
MACROS += ALWAYS_COLLECT
endif

ifeq ($(TYPE),fast-debug)
LDPARAM = 
CCPARAM = -Wall -g
endif

ifeq ($(TYPE),profile)
LDPARAM = -pg /lib/libc.so.5
CCPARAM = -Wall -O2 -pg
MACROS += NDEBUG
endif

ifeq ($(TYPE), release)
LDPARAM = -s
CCPARAM = -Wall -O1
MACROS += NDEBUG
endif

CCPARAM += -std=c++0x

INCPATH = .
LIBPATH = 
C++ = g++
AR = ar crs

EXTRA_FILES = $(INITFILE) init-light.scm test.scm instructions.i Makefile INSTALL README COPYING
STORE = .make-$(TYPE)
PACKAGE_FILES = *.cpp *.hpp README INSTALL COPYING Makefile instructions.i uscheme-init.scm

# Makes a list of the source (.cpp) files.
SOURCE := $(foreach DIR,$(DIRS),$(wildcard $(DIR)/*.cpp))
# List of header files.
HEADERS := $(foreach DIR,$(DIRS),$(wildcard $(DIR)/*.hpp))
# Makes a list of the object files that will have to be created based
# on the source files that have been found and the location of the
# object files that was specified in OBJPATH.
OBJECTS := $(addprefix $(STORE)/, $(SOURCE:.cpp=.o))
# Same for the .d files.
DFILES := $(addprefix $(STORE)/,$(SOURCE:.cpp=.d))

# Specify phony rules.
.PHONY: clean backup dirs tags lib docs install

# Main target.
$(TARGET): dirs $(OBJECTS)
	@echo Linking $(TARGET).
	@$(C++) $(OBJECTS) -o $(TARGET) $(LDPARAM) $(foreach LIBRARY, $(LIBS),-l$(LIBRARY)) $(foreach LIB,$(LIBPATH),-L$(LIB)) $(foreach DLIBRARY, $(DLIBS),$(DLIBRARY).dll)

# Rule for creating object file and .d file, the sed magic is to add
# the object path at the start of the file because the files gcc
# outputs assume it will be in the same dir as the source file.
$(STORE)/%.o: %.cpp
	@echo Creating object file for $*...
	@$(C++) -Wp,-MMD,$(STORE)/$*.dd $(CCPARAM) $(foreach INC,$(INCPATH),-I$(INC))\
                $(foreach MACRO,$(MACROS),-D$(MACRO)) -c $< -o $@
	@sed -e '1s/^\(.*\)$$/$(subst /,\/,$(dir $@))\1/' $(STORE)/$*.dd > $(STORE)/$*.d
	@rm -f $(STORE)/$*.dd

# Empty rule to prevent problems when a header is deleted.
%.hpp: ;

# Cleans up the objects, .d files and executables.
clean:
	@echo Making clean.
	@-rm -f $(foreach DIR,$(DIRS),$(STORE)/$(DIR)/*.d $(STORE)/$(DIR)/*.o)
	@-rm -f $(TARGET)

# Backup the source files.
backup:
	@-if [ ! -e .backup ]; then mkdir .backup; fi;
	@echo Creating backup.
	@zip .backup/backup_`date +%d-%m-%y_%H.%M`.zip $(SOURCE) $(HEADERS) $(EXTRA_FILES)

# Create necessary directories
dirs:
	@-if [ ! -e $(STORE) ]; then mkdir $(STORE); fi;
	@-$(foreach DIR,$(DIRS), if [ ! -e $(STORE)/$(DIR) ]; then mkdir $(STORE)/$(DIR); fi; )

# Create tags file
tags:
	@etags $(foreach DIR, $(DIRS), $(DIR)/*.cpp $(DIR)/*.hpp)

package:
	@echo Creating package unlikely-$(VERSION).zip
	@mkdir unlikely-$(VERSION)
	@cp $(PACKAGE_FILES) unlikely-$(VERSION)
	@zip -r unlikely-$(VERSION).zip unlikely-$(VERSION)/*
	@rm -r unlikely-$(VERSION)

# Custom targets
lib: $(LIBFILE)

LIBOBJECTS = $(filter-out %main.o,$(OBJECTS))

$(LIBFILE): $(LIBOBJECTS)
	@echo Creating library.
	@$(AR) $@ $(LIBOBJECTS)

# Installation
install: $(TARGET) $(LIBFILE)
	@echo Installing Unlikely Scheme.
	@if [ ! -e $(PREFIX)/bin ]; then mkdir -p $(PREFIX)/bin; fi;
	@cp $(TARGET) $(PREFIX)/bin
	@if [ ! -e $(PREFIX)/lib ]; then mkdir -p $(PREFIX)/lib; fi;
	@cp $(LIBFILE) $(PREFIX)/lib
	@if [ ! -e $(PREFIX)/share ]; then mkdir -p $(PREFIX)/share; fi;
	@cp $(INITFILE) $(PREFIX)/share
	@if [ ! -e $(PREFIX)/include/uscheme ]; then mkdir -p $(PREFIX)/include/uscheme; fi;
	@cp $(HEADERS) $(PREFIX)/include/uscheme

# Includes the .d files so it knows the exact dependencies for every
# source.
-include $(DFILES)
