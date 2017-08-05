all:
	corebuild -quiet -classic-display -pkg yojson -pkg async -pkg unix src/main.native

clean:
	corebuild -clean

all-clean: clean all

run: all
	./main.native 5000 2 maps/stock/gen1.json

r1: all
	./main.native 5000 1 maps/stock/gen1.json

.SILENT:
.PHONY: all clean all-clean
