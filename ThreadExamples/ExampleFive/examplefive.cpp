//std::mutex::try_lock() on mutex in c++11 threading
//0. try_lock() tries to lock the mutex. returns immediately. On successful lock axquisition
//returns true otherwise return false.
//1. if try_lock() is not able to lock mutex, then it doesn't get blocked that's why it is called
// non-blocking.
//2. if try_lock is called again by the same thread which owns the mutex, the behavior is
//undefined. it is a dead lock situration with undefined behavour (if you want to able to lock
//the same mutex by same thread more than one time then go for the recursive_mutex)



#include <iostream>
#include <thread>
#include <mutex>

using namespace std;

int counter = 0;
std::mutex mtx;

void increaseTheCounterFor100000Time(){
    for(int i = 0; i < 100000; ++i){
        if(mtx.try_lock()){//non blocking call
            ++counter;
            mtx.unlock();
        }
    }
}
int main(){
    std::thread t1(increaseTheCounterFor100000Time);
    std::thread t2(increaseTheCounterFor100000Time);

    t1.join();
    t2.join();

    cout << " counter could increases upto : " << counter << endl;
    return 0;
}

//there are so many try_lock function
// 1. std::try_lock
// 2. std::mutex::try_lock
// 3. std::shared_lock::try_lock
// 4. std::timed_mutex::try_lock
// 5. std::unique_lock::try_lock
// 6. std::shared_mutex::try_lock
// 7. std::recursive_mutex::try_lock
// 8. std::shared_timed_mutex::try_lock
// 9. std::recursive_time_mutex::try_lock