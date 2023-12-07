cmake_minimum_required(VERSION 3.13)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)
project(NG_FWI VERSION 0.9)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()
message("CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")

add_executable(${PROJECT_NAME} NG_FWI.c util.c)
add_executable(make_daily make_daily.c util.c)
add_executable(make_minmax make_minmax.c util.c)
add_executable(make_hourly make_hourly.c util.c)

IF (NOT WIN32)
    target_link_libraries(${PROJECT_NAME} m)
    target_link_libraries(make_daily m)
    target_link_libraries(make_minmax m)
    target_link_libraries(make_hourly m)
ENDIF()

set(DIR_BIN ${CMAKE_CURRENT_SOURCE_DIR}/bin)

set_target_properties(${PROJECT_NAME} PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY_DEBUG ${DIR_BIN}/
                      RUNTIME_OUTPUT_DIRECTORY_RELEASE ${DIR_BIN}/
)

set_target_properties(make_daily PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY_DEBUG ${DIR_BIN}/
                      RUNTIME_OUTPUT_DIRECTORY_RELEASE ${DIR_BIN}/
)

set_target_properties(make_minmax PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY_DEBUG ${DIR_BIN}/
                      RUNTIME_OUTPUT_DIRECTORY_RELEASE ${DIR_BIN}/
)

set_target_properties(make_hourly PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY_DEBUG ${DIR_BIN}/
                      RUNTIME_OUTPUT_DIRECTORY_RELEASE ${DIR_BIN}/
)
