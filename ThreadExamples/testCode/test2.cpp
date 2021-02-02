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

//Notes for project
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
int nbCars = 0;
int sbCars = 0;

void northBoundTraffic(int num_of_cars, int num_of_cars_allowed_to_pass, string threadDirection,int numCarsOW){
 std::unique_lock<mutex> lock(laneBusy);//automatically calls lock on mutex m1
 std::cout << std::endl;
 //std::cout <<"northbound lane light green" << std::endl;
 std::cout << std::endl;

 srand(time(NULL));

int j = 0;
 while(j <= num_of_cars){ //added '='
    if(num_of_cars == 0){
            oneWayNBLane.wait(lock);
            //break;
        }
     oneWaySBLane.notify_one();
     if(j >= num_of_cars){
         oneWayNBLane.wait(lock);
         //oneWayNBLane.notify_one();
         //break;
     }
     auto lightOn = high_resolution_clock::now();
     std::cout <<"northbound lane light green" << std::endl;///notify lights green
     sleep(rand() % 3 + 1);//simulate cars coming at different times
     int justInCase = num_of_cars - j; //////////////to change N if need be for sequential 
     if( justInCase < num_of_cars_allowed_to_pass){////////////reasign N
        num_of_cars_allowed_to_pass = justInCase;
     }
     for(int i = 0; i < num_of_cars_allowed_to_pass; ++i){
         std::cout << threadDirection << "car " << i << " has passed" << std::endl;//added std:: to first cout
         sleep(rand() % 5);//newly added add + 1 so the range will be 1 -3 and not 0 - 2
     }
     sleep(rand() % 3 + 1);//simulate the space between cars and light transition 
     //std::cout << std::endl;
     std::cout <<"northbound lane light Red" << std::endl;///notify lights red
     auto lightOff = high_resolution_clock::now();
     auto durationOfNorthLightInterval = duration_cast<microseconds>(lightOff - lightOn);
     std::cout << "NorthBound Light Interval Time: " << durationOfNorthLightInterval.count()/1000000 << " Seconds" << std::endl;
     j = j + num_of_cars_allowed_to_pass;
     nbCars = j;
     std::cout << j << std::endl;
     if(j >= num_of_cars){
         break;
     }
     if(sbCars == numCarsOW){
         continue;
     }else{
         oneWayNBLane.wait(lock);
     }
     
     
 }
 //usleep(50);
 std::this_thread::sleep_for(std::chrono::seconds(1));//to prove the cv was waiting
}

void southBoundTraffic(int num_of_cars, int num_of_cars_allowed_to_pass, string threadDirection,int numCarsOW){
 std::unique_lock<mutex> lock(laneBusy);//automatically calls lock on mutex m1
 std::cout << std::endl;
 //std::cout <<"southbound lane light green" << std::endl;
 std::cout << std::endl;
srand(time(NULL));
int j = 0;
 while(j <= num_of_cars){ //added '='
    if(num_of_cars == 0){
        oneWaySBLane.wait(lock);
        //break;
    }
     oneWayNBLane.notify_one();
     if(j >= num_of_cars){
         oneWaySBLane.wait(lock);
         //oneWaySBLane.notify_one();
         //break;
     }
     auto lightOn = high_resolution_clock::now();
     std::cout <<"southbound lane light green" << std::endl;///Notify lights green
     sleep(rand() % 3 + 1);//simulate cars coming at different times
     int justInCase = num_of_cars - j;///////////////////////////to change N if need be for sequential
     if( justInCase < num_of_cars_allowed_to_pass){ /////////////reasign N
        num_of_cars_allowed_to_pass = justInCase;
     }
     for(int i = 0; i < num_of_cars_allowed_to_pass; ++i){
         std::cout << threadDirection << "car " << i << " has passed" << std::endl;//added std:: to first cout
         sleep(rand() % 5);//add + 1 so the range will be 1 -3 and not 0 - 2
     }
     sleep(rand() % 3 + 1);//simulate the space between cars and light transition 
     //std::cout << std::endl;
     std::cout <<"southbound lane light Red" << std::endl;///Notify lights red
     auto lightOff = high_resolution_clock::now();
     auto durationOfSouthLightInterval = duration_cast<microseconds>(lightOff - lightOn);
     std::cout << "SouthBound Light Interval Time: " << durationOfSouthLightInterval.count()/1000000 << " Seconds" << std::endl;

     j = j + num_of_cars_allowed_to_pass;
     sbCars = j;
     std::cout << j << std::endl;
      if(j >= num_of_cars){
         break;
     }
     if(nbCars == numCarsOW){
         continue;
     }else{
         oneWaySBLane.wait(lock);
     }
     
     
 }
 //usleep(50);
 std::this_thread::sleep_for(std::chrono::seconds(1));//to prove the cv was waiting
}

int main(int argc, char *argv[]){
    int num_of_cars = stoi(argv[1]);
    int num_of_cars_allowed_to_pass = stoi(argv[2]);

    srand (time(NULL));
    int num_of_cars1 = rand() % num_of_cars + 1;
    int num_of_cars2 = num_of_cars - num_of_cars1;

    std::cout << "Number of cars assigned to Northbound Lane" << num_of_cars1 << std::endl;
    std::cout << "Number of cars assigned to Southbound Lane" << num_of_cars2 << std::endl;
    
    if(num_of_cars2 == 0){//to prevent a zero case, all lanes will by default start with at least one car in lane
        num_of_cars1 = num_of_cars1 - 1;
        num_of_cars2 = num_of_cars2 + 1;
        std::cout << "Number of cars assigned to Northbound Lane: " << num_of_cars1 << std::endl;
    std::cout << "Number of cars assigned to Southbound Lane: " << num_of_cars2 << std::endl;
    }

    // std::cout << num_of_cars1 << std::endl;
    // std::cout << num_of_cars2 << std::endl;
    string northBoundCars = "northBound ";
    string southBoundCars = "southBound ";

    auto startTime = high_resolution_clock::now();
    auto startTimeNorthBound = high_resolution_clock::now();
    auto startTimeSouthBound = high_resolution_clock::now();

    std::thread northBound(northBoundTraffic, num_of_cars1, num_of_cars_allowed_to_pass, northBoundCars,num_of_cars2);
    std::thread southBound(southBoundTraffic, num_of_cars2, num_of_cars_allowed_to_pass, southBoundCars,num_of_cars1);

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