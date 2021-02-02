#include <iostream>
#include <string>
#include <thread>
#include <vector>
#include <mutex>
#include <condition_variable>
#include <chrono>
#include "unistd.h"

#define MAX 100

using namespace std;

int id = -1;
std::mutex m_mutex;
std::condition_variable intersection;
vector<bool> ready(MAX);

void mainThreadFunction(){
    for(int i = 0; i < MAX; ++i) {
        cout << "releasing thread: " << i << endl;
        std::unique_lock<std::mutex> mlock(m_mutex);
        ready.at(i) = true;
        intersection.notify_one();

        usleep(50);
    }
}

void threadCallback(int x, std::string str)
{
    std::unique_lock<std::mutex> mlock(m_mutex);

    std::cout << "Passed Number = " << x << std::endl;
    std::cout << "Passed String = " << str <<std::endl;

    cout << "thread " << x << " waiting..." << endl;
    ready.at(x) = false;
    while(!ready.at(x)){
        intersection.wait(mlock);
    }
    cout << "thread " << x << " completed!" << endl;
}

int main()
{
    vector<std::thread> vts;

    int x = 10;
    string str = "thread";

    for(int i = 0;i < MAX; ++i){
        vts.push_back(thread(threadCallback, i, str));
    }
    sleep(1);
    thread mainThread(mainThreadFunction);
    mainThread.join();
    for(int i = 0; i < MAX; ++i){
        vts[i].join();
    }
    cout << "done" << endl;
    return 0;
}