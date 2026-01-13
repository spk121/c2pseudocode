# Makefile for unifdef
# Based on Tony Finch's unifdef utility

CC = gcc
CFLAGS = -Wall -Wextra -O2
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
MANDIR = $(PREFIX)/share/man/man1

# Source files
SRCS = unifdef.c
OBJS = $(SRCS:.c=.o)
TARGET = unifdef

# Default target
all: $(TARGET)

# Build the unifdef binary
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

# Compile object files
%.o: %.c unifdef.h version.h
	$(CC) $(CFLAGS) -c $<

# Clean build artifacts
clean:
	rm -f $(TARGET) $(OBJS)

# Install the program and documentation
install: $(TARGET)
	install -d $(BINDIR)
	install -m 755 $(TARGET) $(BINDIR)
	install -d $(MANDIR)
	install -m 644 unifdef.1 $(MANDIR)
	install -m 755 unifdefall.sh $(BINDIR)/unifdefall

# Uninstall
uninstall:
	rm -f $(BINDIR)/$(TARGET)
	rm -f $(BINDIR)/unifdefall
	rm -f $(MANDIR)/unifdef.1

# Test (basic smoke test)
test: $(TARGET)
	@echo "Running basic test..."
	@echo "#ifdef TEST\nyes\n#else\nno\n#endif" | ./$(TARGET) -DTEST | grep -q "yes"
	@echo "Test passed!"

.PHONY: all clean install uninstall test
