# Nome dell'eseguibile
TARGET = main

# Compilatore
CXX = g++

# Flag di compilazione
CXXFLAGS = -std=c++11 -Wall -Wextra -g

# File sorgente
SRCS = main.cpp

# File oggetto
OBJS = $(SRCS:.cpp=.o)

# Regola principale
all: $(TARGET)

# Regola per creare l'eseguibile
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Regola per compilare i file .cpp in .o
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Regola per pulire i file generati
clean:
	rm -f $(OBJS) $(TARGET)

# Regola per pulire tutto, inclusi i file temporanei
dist-clean: clean
	rm -f *~ *.log

# Regola per il debug con Valgrind
valgrind: $(TARGET)
	valgrind --leak-check=full --track-origins=yes ./$(TARGET)

# Regola per eseguire
run: $(TARGET)
	./$(TARGET)

.PHONY: all clean dist-clean valgrind run

