
LIBS = \
	kirk \
	kirk-irram \
	hkirk \
	hkirk-irram

.PHONY: all clean $(LIBS)

all: $(LIBS)

clean:
	for l in $(LIBS); do $(MAKE) -C $$l clean; done

$(LIBS):
	$(MAKE) -C $@ $(TARGET)

kirk-irram: kirk
hkirk: kirk
hkirk-irram: kirk-irram hkirk
