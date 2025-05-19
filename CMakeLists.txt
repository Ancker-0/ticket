cmake_minimum_required(VERSION 3.29)
project(ticket)

set(CMAKE_CXX_STANDARD 20)

add_executable(ticket src/main.cpp
                      src/cmd.h src/typedecl.h src/vector.h)
