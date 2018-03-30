#pragma once

#include <string_view>
#include <vector>

auto read_file_contents(std::string const& filename)
    -> std::vector<char>;
