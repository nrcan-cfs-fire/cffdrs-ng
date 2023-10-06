cmake_minimum_required(VERSION 3.13)
set(CMAKE_C_STANDARD 90)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)
project(ngfwi VERSION 0.9)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()
message("CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")

add_executable(${PROJECT_NAME} NG_FWI.c util.c)
target_link_libraries(${PROJECT_NAME} m)

add_executable(make_daily make_daily.c util.c)
target_link_libraries(make_daily m)

add_executable(make_minmax make_minmax.c util.c)
target_link_libraries(make_minmax m)

add_executable(make_hourly make_hourly.c util.c)
target_link_libraries(make_hourly m)
