#include <iostream>
#include <string>
#include "curl/curl.h"

using namespace std;

static char errorBuffer[CURL_ERROR_SIZE];
static string buffer;
static int writer() {
}

int usage() {
  cout << "curlsample: \n" << endl;
  cout << "\tUsage: curlsample url\n" << endl;
}

int main(int argc, char **argv) {
  if (argc > 1) {
    string url(argv[1]);

    CURL *curl;
    CURLcode result;

    cout << "Retrieving " << url << endl;

    curl = curl_easy_init();

    if(curl) {
      curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errorBuffer);
      curl_easy_setopt(curl, CURLOPT_URL, &url);
      curl_easy_setopt(curl, CURLOPT_HEADER, 0);
      curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1);
      curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writer);
      curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buffer);
    }

  } else {
    usage();
  }
}
