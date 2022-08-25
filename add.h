#include <malloc.h>
#include <memory.h>

namespace MathFuncs
{
    class MyMathFuncs
    {
    public:
        int x;
        int y;

        MyMathFuncs(int x, int y);

        int InstAdd(int z);

        int GetX();

        // Returns a + b
        static double Add(double a, double b);

        // Returns a - b
        static double Subtract(double a, double b);

        // Returns a * b
        static double Multiply(double a, double b);

        // Returns a / b
        static double Divide(double a, double b);
    };
} 
