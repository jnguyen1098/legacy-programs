#include <stdio.h>
#include <math.h>

#define precision 0.000001

double babylon(double N)
{
    double guess = N / 2.0;
    double previous_guess;
    
    do {
        previous_guess = guess;
        guess = (previous_guess + (N / previous_guess))/ 2.0;
    } while (fabs(guess - previous_guess) > precision);
    
    return guess;
}

int main(void)
{
    printf("%f\n", babylon(9458151235.0));
    return 0;
}
