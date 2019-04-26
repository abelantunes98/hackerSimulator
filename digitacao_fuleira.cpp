#include <stdio.h>
#include <string.h>
#include <iostream>
#include <map>
#include <fstream>
#include <bits/stdc++.h>
#include <boost/algorithm/string.hpp>
#include <stdexcept>
#include <stdlib.h>
#include <unistd.h>
using namespace std;
using namespace boost;

int main() {
  string text[6] = {"l", "a", "la", " ", "l", "a"};

  cout << "OI";
  for (int i = 0; i < 6; i++){
    usleep(20000);
	  cout << text[i];
	   if( text[i] == " " )
		   usleep(15000);
  }
  return 0;
}
