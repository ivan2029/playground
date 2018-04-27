#!/usr/bin/python

import os
import sys

#
#
#
def gen_header(names):
    str = "#pragma once\n\n";
    for name in names:
        str += "struct {}{{\n".format(name);
        str += "   void method();\n"
        str += "};\n\n";
    return str


def gen_source(header_name, names):
    str = "#include <{}>\n\n".format(header_name)
    str += "#include <iostream>\n\n"
    for name in names:
        str += "void {0}::method() {{ std::cout << \"{0}\\n\"; }}\n\n".format(name)
    return str


def gen(in_path, out_path_base):
    with open(in_path) as in_file:
        #
        lines = []
        for line in in_file:
            lines.append(line)
            names = list(filter(lambda x: len(x) > 0, map(lambda x: x.strip(), lines)))
        #
        with open(out_path_base + ".hpp", "w+") as out_file:
            header = gen_header(names)
            out_file.write(header)
        #
        with open(out_path_base + ".cpp", "w+") as out_file:
            header_name = os.path.basename(out_path_base + ".hpp")
            source = gen_source(header_name, names)
            out_file.write(source)
            
#
#
#
if __name__ == "__main__":
    gen(sys.argv[1], sys.argv[2])
