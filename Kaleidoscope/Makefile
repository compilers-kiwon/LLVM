kc.out : kc.cpp lexer.h parser.h KaleidoscopeJIT.h
	clang++ -g kc.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o kc.out