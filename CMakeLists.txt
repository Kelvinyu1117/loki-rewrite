cmake_minimum_required(VERSION 3.28)

project(Loki-rewrite LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)

add_library(loki-rewrite)

target_sources(
    loki-rewrite
    PUBLIC
        FILE_SET CXX_MODULES FILES
        loki-rewrite/type_list.cppm
)
