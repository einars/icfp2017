all:
	corebuild -quiet -classic-display src/main.native

clean:
	corebuild -clean

all-clean: clean all

run: all
	./main.native

.SILENT:
.PHONY: all clean all-clean
