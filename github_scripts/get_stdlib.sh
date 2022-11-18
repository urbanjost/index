
git clone https://github.com/fortran-lang/stdlib
cd stdlib
# Build with CMake
cmake -B build

cmake --build build

cmake --build build --target test
# Build with make
# Alternatively, you can build using provided Makefiles:

# make -f Makefile.manual
