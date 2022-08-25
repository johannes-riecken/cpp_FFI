// MathFuncsLib.cpp
// compile with: cl /c /EHsc MathFuncsLib.cpp
// post-build command: lib MathFuncsLib.obj

#include "add.h"
#include <malloc.h>
#include <memory.h>

#include <stdexcept>

using namespace std;

namespace MathFuncs
{
    MyMathFuncs::MyMathFuncs(int x_, int y_) : x(x_), y(y_) {};
    int MyMathFuncs::InstAdd(int z) {
      return x + y + z;
    }

    int MyMathFuncs::GetX() {
      return 5;
    }
    
    double MyMathFuncs::Add(double a, double b)
    {
        return a + b;
    }

    double MyMathFuncs::Subtract(double a, double b)
    {
        return a - b;
    }

    double MyMathFuncs::Multiply(double a, double b)
    {
        return a * b;
    }

    double MyMathFuncs::Divide(double a, double b)
    {
        return a / b;
    }
}
