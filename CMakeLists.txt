# Not required. Just for CLion to work.

cmake_minimum_required(VERSION 3.29)
project(brim)

set(CMAKE_CXX_STANDARD 17)

add_executable(brim
        playground/build/source/src/main.cpp
        playground/build/source/src/test.cpp
)
