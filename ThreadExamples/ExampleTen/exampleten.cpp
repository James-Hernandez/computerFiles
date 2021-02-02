/*
unique_lock in c++ (std::unique_lock<mutex> lock(m1))

1. the class unique_lock is a mutex ownership wrapper.
2. it allows:
    a. can have differenc locking strategies
    b. time-constrained attempts at locking (try_lock_for, try_lock_until)
    c. recursive locking
    d. transfer of lock ownership(move not copy)
    e condition variables. (see this in coming videos)

locking strategies
    Type        Effect(s)
1. derfer_lock  do not acquire owership of the mutex.
2. try_to_lock try to axquire ownership of the mutex without blocking.
3. adopt_lock assume the calling thread already has owernship of the mutex
*/

#include <iostream>
#include <thread>
#include <mutex>

using namespace std;

std::mutex m1;
int buffer = 0;

//Example 1
// void task(const char* threadNumber, int loopfor){
//     std::unique_lock<mutex> lock(m1);//automatically calls lock on mutex m1
//     for(int i = 0; i < loopfor; ++i){
//         buffer++;
//         cout << threadNumber << buffer << endl;
//     }
// }
// int main(){
//     thread t1(task,"T1 ", 10);
//     thread t2(task,"T2", 10);
//     t1.join();
//     t2.join();
//     return 0;
// }

//Example2
void task(const char* threadNumber, int loopFor){
    std::unique_lock<mutex> lock(m1,std::defer_lock);//does not call lock on mutex m1, becaause used defer_lock
    lock.lock();//but then we will have to explicitly tell to lock when ever we want to lock mutex m1
    for(int i = 0; i < loopFor; ++i){
        buffer++;
        cout << threadNumber << buffer << endl;
    }
    //lock.unlock(); is not needed as it will be unlocked in destructor of unique_lock.
}
int main(){
    thread t1(task, "T1 ", 10);
    thread t2(task, "T2 ", 10);
    t1.join();
    t2.join();
    return 0;
}