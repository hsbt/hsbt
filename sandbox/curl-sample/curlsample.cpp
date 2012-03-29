#include <iostream>
#include <string>
using namespace std;

int usage() {
  cout << "curlsample: \n" << endl;
  cout << "\tUsage: curlsample url\n" << endl;
}

int main(int argc, char **argv) {
  if (argc > 1) {
    string url(argv[1]);
    // curl を使う
  } else {
    usage();
  }
}
