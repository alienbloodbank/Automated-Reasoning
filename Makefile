GHC = ghc
SOURCE = Test.hs
OBJECTS = Reasoning.o Reasoning.hi Test.o Test.hi
TARGET = Test
DEPS = Reasoning.hs

default: $(TARGET)

Test: $(SOURCE)
	$(GHC) $(CXXFLAGS) $@

clean:
	rm -f $(OBJECTS) $(TARGET)
