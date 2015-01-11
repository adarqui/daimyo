#include <stdio.h>
#include "prime.h"

/*
testing this function for a friend

    private boolean isPrime(int n)
    {
        boolean isPrime = true;
        
        if (n < 2)
        {
            isPrime = false;
        }
        else
        {
            for (int i = 2; i < n; ++i)
            {
                if (n % i == 0)
                {
                    isPrime = false;
                    break;
                }
            }
        }
        
        return isPrime; 
    }

*/

int is_prime_2(int n) {
    int i;
    unsigned char isPrime = 1;

    if (n < 2) {
        isPrime = 0;
    }
    else {
        for (i = 2; i < n; ++i) {
            if (n % i == 0) {
                isPrime = 0;
                break;
            }
        }
    }

    return isPrime;
}


int is_prime_3(int n) {
    int i;

    if (n < 2) {
        return 0;
    }
    else {
        for (i = 2; i < n; ++i) {
            if (n % i == 0) {
                return 0;
            }
        }
    }
    return 1;
}


int is_prime_4(int n) {
    int i;

    if (n < 2 || n % 2 == 0) {
        return FALSE;
    } else {
        for (i = 2; (i*i) <= n; ++i) {
            if (n % i == 0) {
                return FALSE;
            }
        }
    }
    return TRUE;
}
