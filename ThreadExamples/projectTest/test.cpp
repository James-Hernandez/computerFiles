/*
Name: James Hernandez
ID: 109178400
Description: CSCI 144 Project 2020
*/

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

//after each iteration, make the thread sleep for some random time before notifying the other thread
//or during iteration, have that thread sleep inside the for loop, to simulate cars coming in at random times?
//put sleepers in both outside and inside iterations.
//do the same for keeping track of time for each car passing and lightn interval.
//sleep(takes seconds as parameters) usleep(taskes microseconds as parameters)
//Time Conversions *Note* 1 second == 1000 Millisconds||||| 1 second == 1000000 Microseconds

using namespace std;
using namespace std::chrono;

#define Max 100;

std::mutex laneBusy;
std::condition_variable oneWayNBLane, oneWaySBLane;


void northBoundTraffic(int num_of_cars, int num_of_cars_allowed_to_pass, string threadDirection){
 std::unique_lock<mutex> lock(laneBusy);//automatically calls lock on mutex m1
 std::cout << std::endl;
 //std::cout <<"northbound lane light green" << std::endl;
 std::cout << std::endl;

 srand(time(NULL));

int j = 0;
 while(j < num_of_cars){
     oneWaySBLane.notify_one();
     auto lightOn = high_resolution_clock::now();
     std::cout <<"northbound lane light green" << std::endl;///recently added
     sleep(rand() % 3 + 1);//newly added
     for(int i = 0; i < num_of_cars_allowed_to_pass; ++i){
         std::cout << threadDirection << "car " << i << " has passed" << std::endl;//added std:: to first cout
         sleep(rand() % 5);//newly added add + 1 so the range will be 1 -3 and not 0 - 2
     }
     sleep(rand() % 3 + 1);
     //std::cout << std::endl;
     std::cout <<"northbound lane light Red" << std::endl;///recently added
     auto lightOff = high_resolution_clock::now();
     auto durationOfNorthLightInterval = duration_cast<microseconds>(lightOff - lightOn);
     std::cout << "NorthBound Light Interval Time: " << durationOfNorthLightInterval.count()/1000000 << " Seconds" << std::endl;
     j = j + num_of_cars_allowed_to_pass;
     std::cout << j << std::endl;
     if(j >= num_of_cars){
         break;
     }
     oneWayNBLane.wait(lock);
     
 }
 //usleep(50);
 std::this_thread::sleep_for(std::chrono::seconds(1));//to prove the cv was waiting
}

void southBoundTraffic(int num_of_cars, int num_of_cars_allowed_to_pass, string threadDirection){
 std::unique_lock<mutex> lock(laneBusy);//automatically calls lock on mutex m1
 std::cout << std::endl;
 //std::cout <<"southbound lane light green" << std::endl;
 std::cout << std::endl;
srand(time(NULL));
int j = 0;
 while(j < num_of_cars){
    
     oneWayNBLane.notify_one();
     auto lightOn = high_resolution_clock::now();
     std::cout <<"southbound lane light green" << std::endl;///recently added
     sleep(rand() % 3 + 1);//newly added
     for(int i = 0; i < num_of_cars_allowed_to_pass; ++i){
         std::cout << threadDirection << "car " << i << " has passed" << std::endl;//added std:: to first cout
         sleep(rand() % 5);//newly added
     }
     sleep(rand() % 3 + 1);//newly added
     //std::cout << std::endl;
     std::cout <<"southbound lane light Red" << std::endl;///recently added
     auto lightOff = high_resolution_clock::now();
     auto durationOfSouthLightInterval = duration_cast<microseconds>(lightOff - lightOn);
     std::cout << "SouthBound Light Interval Time: " << durationOfSouthLightInterval.count()/1000000 << " Seconds" << std::endl;

     j = j + num_of_cars_allowed_to_pass;
     
     std::cout << j << std::endl;
      if(j >= num_of_cars){
         break;
     }
     oneWaySBLane.wait(lock);
     
 }
 //usleep(50);
 std::this_thread::sleep_for(std::chrono::seconds(1));//to prove the cv was waiting
}

int main(int argc, char *argv[]){
    int num_of_cars = stoi(argv[1]);
    int num_of_cars_allowed_to_pass = stoi(argv[2]);

    string northBoundCars = "northBound ";
    string southBoundCars = "southBound ";

    auto startTime = high_resolution_clock::now();
    auto startTimeNorthBound = high_resolution_clock::now();
    auto startTimeSouthBound = high_resolution_clock::now();

    std::thread northBound(northBoundTraffic, num_of_cars, num_of_cars_allowed_to_pass, northBoundCars);
    std::thread southBound(southBoundTraffic, num_of_cars, num_of_cars_allowed_to_pass, southBoundCars);

    std::this_thread::sleep_for(std::chrono::seconds(2));//to prove the cv was waiting

    if(northBound.joinable()){
        northBound.join();
    }

    auto stopTimeNorthBound = high_resolution_clock::now();

    if(southBound.joinable()){
        southBound.join();
    }

    auto stopTimeSouthBound = high_resolution_clock::now();

    auto stopTime = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stopTime - startTime);
    auto durationNorthBound = duration_cast<microseconds>(stopTimeNorthBound - startTimeNorthBound);
    auto durationSouthBound = duration_cast<microseconds>(stopTimeSouthBound - startTimeSouthBound);

    std::cout << std::endl;
    std::cout << "All cars have finished passing" << std::endl;
    std::cout << "Both sides of Bridge are clear" << std::endl;
    std::cout << "Time for both lanes to be cleared: " << duration.count()/1000000 << " Seconds" << std::endl;
    std::cout << "Time for Northbound Lane to be cleared: " << durationNorthBound.count()/1000000 << " Seconds" << std::endl;
    std::cout << "Time for Southbound Lane to be cleared: " << durationSouthBound.count()/1000000 << " Seconds" << std::endl;
    // std::cout << num_of_cars << std::endl;
    // std::cout << num_of_cars_allowed_to_pass << std::endl;
    return 0;
}