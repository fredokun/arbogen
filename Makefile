TEMPORARY_DIR=_obuild
ARBOGEN_BIN_DIR_TMP=$(TEMPORARY_DIR)/arbogen
ARBOGEN_BIN_TMP=$(ARBOGEN_BIN_DIR_TMP)/arbogen.asm $(ARBOGEN_BIN_DIR_TMP)/arbogen.byte
ARBOGEN_BIN_DIR=bin

all : main

main :
	ocp-build -init
	ocp-build build arbogen
	mkdir -p bin
	cp $(ARBOGEN_BIN_TMP) $(ARBOGEN_BIN_DIR)

clean :
	ocp-build clean
	rm bin/arbogen.*
