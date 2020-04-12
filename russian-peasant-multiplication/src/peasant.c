/*****************************************************************************/
/*                    Russian Peasant Multiplication in C                    */
/*                         By Jason Nguyen (XXXXXXXX)                         */
/*                                 XXXXXXXX                                  */
/*****************************************************************************/
#include <stdio.h>
#include <unistd.h>
#include <time.h>

/* recursive russian peasant multiplication */
unsigned long long rpmRecursive(long long m, long long n);

/* iterative russian peasant multiplication */
unsigned long long rpmIterative(long long m, long long n);

/* benchmarking function for timing recursive func */
double benchmark_recursive(long long m, long long n);

/* benchmarking function for timing iterative func */
double benchmark_iterative(long long m, long long n);

int main(void)
{
    // user input
    long long m, n;

    // to store the answers
    unsigned long long recursiveAnswer;
    unsigned long long iterativeAnswer;

    puts("");
    puts("--------------------------------------------");
    puts("--   Russian Peasant Multiplication in C  --");
    puts("--     Enter a negative number to quit    --");
    puts("--        by Jason Nguyen (XXXXXXXX)       --");
    puts("--------------------------------------------");

    sleep(1); // for animation's sake...
    printf("Please wait...running startup benchmarks...\n");

    // number ranges used for testing
    int rng[] = {1, 5, 50, 500, 5000, 0};

    // iterating over the tests
    for (int i = 1; rng[i]; i++) {
        sleep(1);
        printf("\nMultiplying every number from %d to %d...\n", rng[i - 1], rng[i]);
        sleep(1);
        // calling the test functions and printing results
        printf("    Recursion: %.6f seconds\n",
                benchmark_recursive(rng[i - 1], rng[i]));
        printf("    Iterative: %.6f seconds\n",
                benchmark_iterative(rng[i - 1], rng[i]));
    }

    // heh..
    sleep(1);
    printf("\nBenchmarks complete!\n");

    do {
        // user input for the first number
        printf("\nEnter a positive number (or negative to quit):\n> ");
        scanf("%lld", &m);

        // exit if negative
        if (m < 0) break;

        // user input for the second number
        printf("\nEnter another one (again, negative to quit):\n> ");
        scanf("%lld", &n);

        // exit if negative
        if (n < 0) break;

        // create recursive solution
        recursiveAnswer = rpmRecursive(m, n);

        // create iterative solution
        iterativeAnswer = rpmIterative(m, n);

        // print them separately
        printf("\n(recursive) %lld * %lld = %llu  ", m, n, recursiveAnswer);
        printf("\n(iterative) %lld * %lld = %llu\n", m, n, iterativeAnswer);
    } while (m >= 0 && n >= 0); // if user enters a negative, we stop looping

    // we are done
    puts("\nThanks for using this program!");
    return 0;
}

unsigned long long rpmIterative(long long m, long long n)
{
    /* the variable we return */
    unsigned long long result = 0;

    /* repeatedly divide m while doubling n, until m is reduced
     * to zero completely. every time m happens to be odd, we
     * compensate for the rounding error by adding a copy of
     * it to the result. */
    while (m > 0) {
        if (m % 2 == 1)
            result += n;
        m /= 2;
        n *= 2;
    }

    // we are done
    return result;
}

unsigned long long rpmRecursive(long long m, long long n)
{
    /* base case */
    if (m == 0)
        return 0;
    if (m == 1)
        return n;

    /* recursive/inductive cases: every time m is odd, we
     * add n to the final expression containing the call*/
    if (m % 2 == 0)
        return rpmRecursive(m / 2, n * 2);
    else
        return n + rpmRecursive(m / 2, n * 2);
}

double benchmark_recursive(long long m, long long n)
{
    // timing
    clock_t start = clock();
    for (int i = m; i <= n; i++)
        for (int j = m; j <= n; j++)
            rpmRecursive(i, j);
    return (((double)(clock() - start)) / CLOCKS_PER_SEC);
}

double benchmark_iterative(long long m, long long n)
{
    // timing
    clock_t start = clock();
    for (int i = m; i <= n; i++)
        for (int j = m; j <= n; j++)
            rpmIterative(i, j);
    return (((double)(clock() - start)) / CLOCKS_PER_SEC);
}
