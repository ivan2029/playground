/*
	https://open.kattis.com/problems/honey
*/

#include <iostream>
#include <iomanip>
#include <vector>

std::vector<int> walks_memo(29 * 29 * 15, -1);

/*
	x in [-14, 14]
	y in [-14, 14]
	s in [  0, 14]
*/
int& w(int x, int y, int s) {
	return walks_memo[(x + 14)*29*29 + (y + 14)*29 + s];
}
 
int compute_w(int x, int y, int s) {
	if( w(x, y, s) == -1 ) {
		w(x, y, s) = 
			compute_w(x - 1,     y, s - 1)
		+ compute_w(x + 1,     y, s - 1)
		+ compute_w(    x, y - 1, s - 1)
		+ compute_w(    x, y + 1, s - 1)
		+ compute_w(x - 1, y - 1, s - 1)
		+ compute_w(x + 1, y + 1, s - 1);
	}
	return w(x, y, s);
}

void precompute() {
	for(int i = -14; i < 15; ++ i) {
		for(int j = -14; j < 15; ++ j) {
			w(i, j, 0) = 0;
		}
	}
	w(0,0,0) = 1;
	for(int i = 0; i < 15; ++ i) {
		compute_w(0, 0, i);
	}
}
 
void run(std::istream& in) {
	precompute();
    
	int test_count{0};
	in >> test_count;
	for(int i = 0; i < test_count; ++ i) {
		int len{0};
		in >> len;
		std::cout << w(0, 0, len) << "\n";
	}
}

#if defined(TEST_VERSION)
#include <sstream>

int main() {
	std::stringstream str( "14 1 2 3 4 5 6 7 8 9 10 11 12 13 14" );
	run(str);
	return 0;
}

#else
int main () {
	run(std::cin);
	return 0;
}
#endif
