cmake_minimum_required(VERSION 3.11)
project(Demo)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_EXPORT_COMPILE_COMMANDS True)

find_package(OpenGL REQUIRED)
find_package(GLEW REQUIRED)
find_package(glfw3 REQUIRED)

add_executable(demo src/demo.cpp)
target_link_libraries(demo OpenGL OpenGL::GL GLEW::GLEW glfw)
target_include_directories(demo PUBLIC "${PROJECT_BINARY_DIR}")

file(READ src/shader.vert SHADER_VERT)
file(READ src/shader.frag SHADER_FRAG)
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS src/shader.vert)
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS src/shader.frag)
configure_file(src/shader.hpp.in shader.hpp)
