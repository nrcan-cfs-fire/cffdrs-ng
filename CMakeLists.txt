cmake_minimum_required(VERSION 3.13)
set(CMAKE_C_STANDARD 90)
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

set(DIR_BIN STRING ${CMAKE_CURRENT_SOURCE_DIR}/bin)

add_custom_target(build-time-make-directory ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${DIR_BIN})

add_custom_command(TARGET ${PROJECT_NAME}
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${PROJECT_NAME}> ${DIR_BIN})
add_custom_command(TARGET make_daily
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:make_daily> ${DIR_BIN})
add_custom_command(TARGET make_minmax
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:make_minmax> ${DIR_BIN})
add_custom_command(TARGET make_hourly
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:make_hourly> ${DIR_BIN})
