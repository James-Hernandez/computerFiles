//Mutex mutual exclusion
//mutex is used to avoid race conditions
//we use lock(), unlock() on mutex to avoid race condition

#include <iostream>
#include <thread>
#include <mutex>

using namespace std;

int myAmount = 0;
std::mutex m;
void addMoney(){
    m.lock();
    ++myAmount;//critical section
    m.unlock();
}

int main() {
    std::thread t1(addMoney);
    std::thread t2(addMoney);

    t1.join();
    t2.join();

    cout << myAmount << endl;
    return 0;
}