cmake_minimum_required(VERSION 2.8.0)
enable_language(Fortran)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/inc)
set(include_directories "${include_directories}${CMAKE_BINARY_DIR}/inc")
project(GLP)
find_package(OpenGL REQUIRED)
find_package(GLUT REQUIRED)
find_package(X11 REQUIRED)
include_directories(${OPENGL_INCLUDE_DIRS} ${GLUT_INCLUDE_DIRS} ${X11_INCLUDE_DIR})
set(AF03GL_SRCS ${AF03GL_SRCS}
	AF03GL/FreeGLUT.h
	AF03GL/GL.h
	AF03GL/GLU.h
	AF03GL/GLUT_fonts.c
	AF03GL/OpenGL_gl.f90
	AF03GL/OpenGL_glu.f90
	AF03GL/OpenGL_freeglut.f90
	AF03GL/f03gl_gl.f90
	AF03GL/f03gl_glu.f90
)
set(PLOTTER_SRCS ${PLOTTER_SRCS}
	Plotter/src/plotter.f90
)
set(PLOTTER_TST ${PLOTTER_TST}
	Plotter/tst/test_function_many_basic.f90
	Plotter/tst/test_function_many_discontinuous.f90
	Plotter/tst/test_function_one_basic.f90
	Plotter/tst/test_function_one_discontinuous.f90
	Plotter/tst/test_points_many_basic.f90
	Plotter/tst/test_points_many_discontinuous.f90
	Plotter/tst/test_points_many_perforated.f90
	Plotter/tst/test_points_one_basic.f90
	Plotter/tst/test_points_one_discontinuous.f90
	Plotter/tst/test_points_one_perforated.f90
	Plotter/tst/test_surfaces_many.f90
	Plotter/tst/test_surfaces_one.f90
)
add_library(AF03GL STATIC ${AF03GL_SRCS})
add_library(Plotter STATIC ${PLOTTER_SRCS})
set_target_properties(
	AF03GL PROPERTIES
	COMPILE_FLAGS
		"-DOPENGL -fno-range-check -lpthread -ldl -lgfortran ${C2_FLAGS}"
)

target_link_libraries(AF03GL ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES} ${X11_LIBRARIES})
target_link_libraries(Plotter ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES} AF03GL)

foreach(test_source_file ${PLOTTER_TST})
	string(REPLACE ".f90" "" test_target_name ${test_source_file})
	string(REPLACE "Plotter/tst/" "" test_target_name ${test_target_name})
	add_executable(${test_target_name} ${test_source_file})
	target_link_libraries(${test_target_name} Plotter AF03GL ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})
	set_target_properties(${test_target_name} PROPERTIES SUFFIX ".gce")
endforeach(test_source_file ${PLOTTER_TST})
