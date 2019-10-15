GHC = ghc
SOURCE = Test.hs
OBJECTS = $(wildcard *.hi *.o)
TARGET = Test

default: $(TARGET)

Test: $(SOURCE)
	$(GHC) $<

clean:
	rm -f $(OBJECTS) $(TARGET)
