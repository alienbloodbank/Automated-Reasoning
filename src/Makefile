GHC = ghc
SOURCES = $(wildcard *.hs)
OBJECTS = $(wildcard *.hi *.o)
DRIVER = Test.hs
TARGET = Test

default: $(TARGET)

Test: $(SOURCES)
	$(GHC) $(DRIVER)

clean:
	rm -f $(OBJECTS) $(TARGET)
