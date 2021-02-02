/*
Condition variables
Important point: CV are used for two purpose
A.Notify other threads
B.Waiting for some conditions

1.Condition VAriable allows running threads to wait on some conditions and once those conditions
are met the waiting thread is notified using :
    a. notify_one();
    b. notify_all();

2. you need mutex to use conditon variable.
3. if some thread want to wait on some condition then it has to do these things:
    a. Acquire the mutex lock using std::unique_lock<std::mutex> lock(m);
    b. Execute wait, wait_for, or wait_until. the wait operates atomically release the mutex
    and suspend the execution of the thread.
    c. when the condition variable is notified, the thread is awakened, and the mutex is atomically
    reacquired. the thread should then check the condition and resume waiting if the wake up
    was spurious.

Note:
1. Condition variables are used to sychonized two or more threads.
2. Best use case of conditon variable is Producer/ Consumer problem.
*/
#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>
using namespace std;

std::condition_variable cv;
std::mutex m;
long balance = 0;

void addMoney(int money){
    std::lock_guard<mutex> lg(m);
    balance += money;
    cout << "Amount Added Current Balance: " << balance << endl;
    cv.notify_one();
}

void withdrowMoney(int money){
    std::unique_lock<mutex> ul(m);
    cv.wait(ul, []{return(balance != 0) ? true : false;});
    if(balance >= money){
        balance -= money;
        cout << "Amount Deducted: " << money << endl;
    }else{
        cout << "amount cant be deducted, current balance is less than " << money << endl;
    }
    cout << "current balance is: " << balance << endl;
}
int main(){
    std::thread t1(withdrowMoney, 500);
    std::this_thread::sleep_for(std::chrono::seconds(2));//to prove the cv was waiting
    std::thread t2(addMoney, 500);
    t1.join();
    t2.join();
    return 0;
}