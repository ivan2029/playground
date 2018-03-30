#include "read_file_contents.hpp"

#include <fstream>
#include <iterator>


auto read_file_contents(std::string const& filename)
    -> std::vector<char>
{
    std::ifstream file{filename, std::ios::binary | std::ios::ate};

    auto const size = file.tellg();
    file.seekg(0);

    std::vector<char> contents(size, '\0');
    file.read(contents.data(), size);

    return contents;
}

