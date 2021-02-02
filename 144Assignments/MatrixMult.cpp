#include <iostream>
#include <thread>
#include <random>
#include <chrono>
#include <mutex>
#include <chrono>
#include <stdlib.h>
#include <stdio.h>
#include <functional>
#include <windows.h>

using namespace std;

#define Msize 4      //Size of Matrix
#define Max_Thread 4 //Number of threads we will have

int step_i = 0; //an iterator to help with looping later.
mutex mtx;      //declared a mutex for locking just in case

struct Matrix
{
    int **elem;                     //pair (i,j)
    void initialize_random_arrays() //to initalize Matrix A and B with random #'s
    {
        elem = new int *[Msize];
        for (int i = 0; i < Msize; ++i)
        {
            elem[i] = new int[Msize];
            for (int j = 0; j < Msize; ++j)
            {
                elem[i][j] = rand() % 10;
            }
        }
    };
    void initialize_zero_arrays() //to initialize our Resulting Matrix with zeros cause upon declaration we do not know what is in there
    {
        elem = new int *[Msize];
        for (int i = 0; i < Msize; ++i)
        {
            elem[i] = new int[Msize];
            for (int j = 0; j < Msize; ++j)
            {
                elem[i][j] = 0;
            }
        }
    };
    void print_arrays() //A simple print arrays for the matrix
    {
        cout << endl;
        for (int i = 0; i < Msize; ++i)
        {
            for (int j = 0; j < Msize; ++j)
            {
                cout << elem[i][j] << " ";
            }
            cout << endl;
        }
        cout << endl;
    };
};

//Have our function definitions before main to inform compiler.
void test_Struct(Matrix &r, Matrix &m1, Matrix &m2);                              //to test our structure to make sure it is working properly
void matrix_Mult(Matrix &r, Matrix &m1, Matrix &m2);                              //to alert multiply matrix
void multiplyMatrix(Matrix &r, const int tn, const Matrix &m1, const Matrix &m2); //to actually multiply the matrixis

int main()
{

    Matrix r, m1, m2;
    //test_Struct(r,m1,m2);
    matrix_Mult(r, m1, m2);                 //we call matrix mult with our initial matrix
    cout << "finished main thread" << endl; //so that we can see when full program is finished
    return 0;
}

void test_Struct(Matrix &r, Matrix &m1, Matrix &m2) //just to see if structure works as intended for later code
{
    //Matrix r, m1, m2;
    r.initialize_zero_arrays();
    r.print_arrays();
    m1.initialize_random_arrays();
    m1.print_arrays();
    m2.initialize_random_arrays();
    m2.print_arrays();
}
void matrix_Mult(Matrix &r, Matrix &m1, Matrix &m2)
{
    //Matrix r, m1, m2;
    r.initialize_zero_arrays();
    r.print_arrays();
    m1.initialize_random_arrays();
    m1.print_arrays();
    m2.initialize_random_arrays();
    m2.print_arrays();

    std::thread threads[Max_Thread];
    for (int i = 0; i < Max_Thread; ++i)//loop to creat threads
    {
        cout << "creating thread" << i << endl;
        threads[i] = std::thread(multiplyMatrix, std::ref(r), i, std::ref(m1), std::ref(m2));
    }

    for (int j = 0; j < Max_Thread; ++j)//loop to join threads in order
    {
        cout << "thread" << j << "joining" << endl;
        threads[j].join();
    }
    r.print_arrays();//print final result of matrix
    //     threads[0].join();
    //     threads[1].join();
    //     threads[2].join();
    //     threads[3].join();
}
void multiplyMatrix(Matrix &r, const int tn, const Matrix &m1, const Matrix &m2)
{

    int section = step_i++;
    for (int i = section * Msize / 4; i < (section + 1) * Msize / 4; i++)//we have it so each thread only computes a quarters worth of the matrix
    {
        for (int j = 0; j < Msize; j++)
        {
            for (int k = 0; k < Msize; k++)
            {
                r.elem[i][j] += m1.elem[i][k] * m2.elem[k][j];
            }
        }
    }
}