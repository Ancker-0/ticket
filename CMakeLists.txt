cmake_minimum_required(VERSION 3.29)
project(ticket)

set(CMAKE_CXX_STANDARD 20)

add_executable(ticket src/main.cpp
        src/fs-ext.cpp
        src/fs-ext.h
        src/fs.cpp
        src/fs.h
        src/util.h
        src/error.h)
