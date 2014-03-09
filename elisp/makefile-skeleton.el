;;Makefile skeleton
;;Written by Chris Konstad
;;2014-03-08
;;Released under the MIT license
;;chriskon149@gmail.com

;;Makefile skeleton
(define-skeleton makefile-skeleton
  "Insert a Makefile skeleton"
  "Executable Name: "
  "# This is a Makefile skeleton.\n"
  "# You must fill out the details.\n"
  "CC=g++\n"
  "CFLAGS=-c -Wall -Wextra -g\n"
  "LDFLAGS=\n"
  "# Add source files here\n"
  "SOURCES="_"\n"
  "OBJECTS=$(SOURCES:.cpp=.o)\n"
  "EXECUTABLE="str"\n"
  "\n"
  "all: $(SOURCES) $(EXECUTABLE)\n"
  "\n"
  "$(EXECUTABLE): $(OBJECTS)\n"
  "\t$(CC) $(LDFLAGS) $(OBJECTS) -o $@\n"
  "\n"
  ".cpp.o:\n"
  "\t$(CC) $(CFLAGS) $< -o $@\n"
  "\n"
  "clean:\n"
  "\trm -rf *.o $(EXECUTABLE) $(EXECUTABLE).dSYM\n"
)
