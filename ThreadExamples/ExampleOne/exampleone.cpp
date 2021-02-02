#include <iostream>
#include <vector>
#include <stdlib.h>
#include <thread>
#include <algorithm>
using namespace std;
using namespace std::chrono;

typedef unsigned long long ull;
ull OddSum = 0;
ull EvenSum = 0;

void findEven(ull start, ull end){
    for (ull i = start; i <= end; i++){
        if((i & 1) == 0){
            EvenSum += i;
        }
    }
}
void findOdd(ull start, ull end){
    for (ull i = start; i <= end; i++){
        if((i & 1) == 1){
            OddSum += i;
        }
    }
}

int main(){
    ull start = 0, end = 1900000000;
    auto startTime = high_resolution_clock::now();

    std::thread t1(findEven, start, end);
    std::thread t2(findOdd, start, end);


    t1.join();
    t2.join();

    // findOdd(start, end);
    // findEven(start, end);
    std::this_thread::sleep_for(std::chrono::seconds(2));////newly added line, can just take out later
    auto stopTime = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stopTime - startTime);

    cout << "odd sum: " << OddSum << endl;
    cout << "Even sum : " <<EvenSum << endl;
    cout << "Sec: " <<  duration.count()/1000000 << endl;
    return 0;
}