#include <iostream>
#include <string>
#include <thread>
#include <vector>
#include <mutex>
#include <algorithm>
#include <stdlib.h>
#include <condition_variable>
#include <chrono>
#include "unistd.h"

using namespace std;
using namespace std::chrono;
//rand() % 10 gives numbers from 0 - 9 
//rand() % 10 + 1 gives number from 1 - 10
int main(int argc, char *argv[]){
    int randomNum = 0;

    int num_From_Command_Line = stoi(argv[1]);
    std::cout << num_From_Command_Line << std::endl;

    srand (time(NULL));
    // for(int i = 0; i < num_From_Command_Line; i++) {
    //     randomNum = rand() % 10 + 1;
    //     std::cout << randomNum << std::endl;
    // }
    int random1 = rand() % num_From_Command_Line + 1;
    int random2 = num_From_Command_Line - random1;
    int randomTotal = random1 + random2;

    std::cout << "random1 " << random1 << std::endl;
    std::cout << "random2 " << random2 << std::endl;
    std::cout << "randomTotal " << randomTotal << std::endl;
// auto startTime = high_resolution_clock::now();
// sleep(rand() % 5);
// auto stopTime = high_resolution_clock::now();
// auto duration = duration_cast<microseconds>(stopTime - startTime);
// std::cout << "Time for sleepting is: " << duration.count()/1000000 << " Seconds" << std::endl;
    return 0;
}