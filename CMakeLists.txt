cmake_minimum_required(VERSION 3.29)
project(ticket)

set(CMAKE_CXX_STANDARD 20)

add_executable(code src/main.cpp
                      src/cmd.h src/typedecl.h src/vector.h
                      src/db/config.h src/db/database.h src/db/database_more.h src/db/fs.cpp src/db/fs.h src/db/util.h src/db/error.h
        src/account.cpp
        src/account.h
        src/trainer.cpp
        src/trainer.h
        src/db/fs_vector.cpp
        src/db/fs_vector.h
        src/hasher.h
        src/transfer.cpp
        src/transfer.h)
