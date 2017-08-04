all:
	corebuild -quiet -classic-display -pkg yojson src/main.native

clean:
	corebuild -clean

all-clean: clean all

run: all
	./main.native 2 maps/stock/gen1.json

.SILENT:
.PHONY: all clean all-clean
