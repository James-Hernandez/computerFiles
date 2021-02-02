#include <iostream>
#include <chrono>
#include <thread>

using namespace std;

void run(int count){
    while (count --> 0){
        cout << "CppNuts" << endl;
    }
    std::this_thread::sleep_for(chrono::seconds(3));
}

int main(){
    std::thread t(run, 10);
    cout << "main()" << endl;
    if(t.joinable())
        t.join();
    cout << "main() after" <<endl;
    return 0;
}