rm -rf build && mkdir build && cd build
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -GNinja ..
ninja -j 16
