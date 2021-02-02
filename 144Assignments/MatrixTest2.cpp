#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <mutex>
#include <thread>

//NOTE: THIS CODE NEEDS C++11 COMPILER TO COMPILE.

using namespace std;
int const size = 4;
mutex mtx[size][size];
thread myThread[size];


void MatrixMultiplication(int const * const * matrixA, int const * const * matrixB,int** matrixC, int row, int col, int i){
    mtx[row][col].lock();
    matrixC[row][col] += matrixA[row][i]*matrixB[i][col];
    mtx[row][col].unlock();
}

int main()
{
    int** matrixA;
    int** matrixB;
    int** matrixC;

    matrixA = new int*[size];
    matrixB = new int*[size];
    matrixC = new int*[size];

    for(int i = 0; i<size; i++){
        matrixA[i] = new int[size];
        matrixB[i] = new int[size];
        matrixC[i] = new int[size];
    }

/*[1,2,3,4]
[5,6,7,8]
[.....]
[.....]

*/
    //WE ARE INITIALIZING MATRICES.
    int count = 1;
    for(int i = 0; i<size; i++){
        for(int j = 0; j<size; j++){
            matrixA[i][j] = count;
            matrixB[i][j] = count;
            count++;
        }
    }


//    for(int row=0; row<size; row++){
//         for(int col=0; col<size;col++){
//             for(int i=0;i<size;i++){
//                 myThread[i] = thread(MatrixMultiplication, matrixA, matrixB, matrixC, row, col, i);
//                 myThread[i].join();
//             }
//         }
//     }

for(int row=0; row<size; row++){
        for(int col=0; col<size;col++){
            for(int i=0;i<size;i++){
                myThread[i] = thread(MatrixMultiplication, matrixA, matrixB, matrixC, row, col, i);
            }
        }
    }

for(int row=0; row<size; row++){
   for(int col=0; col<size;col++){
      for(int i=0;i<size;i++){
         myThread[i].join();
      }
   }
}

    for(int i = 0; i<size; i++){
        for(int j = 0; j<size; j++){
            cout<<matrixC[i][j]<<"  ";
        }
        cout<<endl;
    }

    return 0;
}