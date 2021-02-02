#include<iostream>
#include<thread>

using namespace std;
//1.function pointer
// void fun(int x) {
//     while (x --> 0){
//         cout << x << endl;
//     }
// }

// int main() {
//     std::thread t1(fun, 10);
//     t1.join()

//     return 0;
// }

// 2. lambda function
//we can directly inject lambda at thread creation time
// int main() {
//     auto fun = [](int x){
//         while (x --> 0){
//             cout << x << endl;
//         }
//     };
//     std::thread t(fun,10);
//     t.join();
//     return 0;
// }
//after directly injecting it looks like this
// std::thread t([](int x){
//          while (x --> 0){
//            cout << x << endl;         }     },10);

//3. functor (function object)
// class Base {
//     public:
//         void operator ()(int x) {
//             while (x --> 0){
//                 cout << x << endl;
//             }
//         }
// };
// int main() {
//     std::thread t((Base()),10);
//     return 0;
// }
//4 non static member function
// class Base
// {
//     public:
//         void fun(int x){
//             while(x --> 0){
//                 cout << x << endl;
//             }
//         }
// };
// int main(){
//     Base b;
//     std::thread t(&Base::fun,&b,10);
//     return 0;
// }
//5 static member function
// class Base{
//     public: 
//         static void run(int x){
//             while(x --> 0){
//                 cout << x << endl;
//             }
//         }
// };
// int main(){
//     std::thread t(&Base::run,10);
//     t.join();
//     return 0;
// }



