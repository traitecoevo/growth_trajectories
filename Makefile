TARGETS= ms/ms.pdf

all: $(TARGETS)

maker:
	maker all

ms/ms.pdf: maker
	make -C ms

clean:
	maker clean
	rm -f $(TARGETS)

.PHONY: all clean maker
