#include "app.hpp"

#include <iostream>

auto main(int argc, char** argv) -> int {
    App app{argc, argv};

    try {
        app.run();
    } 
    catch( std::exception const& e ) {
        std::cerr << e.what() << "\n";
    }
    catch( ... ) {
        std::cerr << "unknown error\n";
    }

    return 0;
}
