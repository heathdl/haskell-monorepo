Just messing around.

# What's here?

My first Haskell programs are various sorting algorithms (`Sort` directory), binary search and bisection search. Don't look at them. Look at these instead!

## Primes

Prime number related programs, there are:

-   an infinite list of primes
-   prime factorisation of an arbitrary number
-   an infinite pairwise list of numbers to their prime factorisations
-   coprimality testing (and calculation of all coprimes to some number)
-   Euler and Carmichael totient functions
-   Miller-Rabin primality tests
    -   deterministic test for <= 2^64
    -   non-deterministic test for any integral
    -   a function for getting the next highest prime after some integral input
    -   a function for an infinite list of primes > some integral input

## Sequence

Several sequences (a lot are in the OEIS, some are fast enough to calculate more terms than you can find on the internet!). They include:

-   Carmichael numbers (this is slow, it is a bruteforce witness check of the number's coprimes, I don't know if there is a better way, haven't really looked)
-   Catalan numbers, uses the recurrence relation, not the closed form
-   Fermat numbers & primes. Fermat numbers are just `2^n - 1`, fermatPrimes are filtered for the first 5 fermatNumbers that are prime.
-   Happy numbers, all numbers that when repeatedly summing the squares of their digits eventually reach 1.
    -   Also includes an infinite list of where each number starts their loop, along with a list of unhappy numbers.
-   Mersenne numbers & primes, computes Mersenne primes with the Lucas-Lehmer primality test, exposes both the exponents and the primes themselves as separate lists.
-   Perfect numbers, uses Mersenne primes so it is relatively fast.
-   Freestyle perfect numbers, an abuse of the sum of divisors formula with composite factors included, basically fake perfect numbers.
-   Palindromic numbers, these sequences don't _generate_ the palindrome, it is just two sequences of numbers which are palindromic in _no_ bases, and numbers that are palindromic in more bases than any number less than them.
-   Binary hamming weight, produces an (ordered) infinite list of numbers whose binary expansion has `n` 1s.

## Approximations

Infinite progressively more precise rational approximation lists, I have one for √2 using Pell numbers (faster to compute than Newton's method), and another for Π using Ramanujan's Π formula.

Comes with a way for formatting as an infinite base 10 string (compares equality of two sequential rational approximations' decimal expansion).

There are examples later, the algorithms can compute way more digits, but I didn't want to flood this README with loads of decimal digits!

## Miscellaneous Programs

-   'Sparsely' smooth numbers, not a real mathematical concept, this is a generate of numbers whose prime factorisation only contains primes from an input list, smooth numbers are sort of like this, but the idea is their prime factorisations only contain numbers less than some number.
-   Trigonometry, I like this one, the algorithm for computing sin & cos is of my own derivation, it uses angle doubling & halving formulas to compute sin & cos for each bit of the binary expansion of a number, then sums it.
-   RSA, not cryptographically secure whatsoever, was trivial to implement given all the functions I have around primes and exponentiation.
-   Numerical palindromes, has loads of functions (which don't depend on each other as much as you'd think!):
    -   Infinite list of palindromes in base `b`
    -   Next highest palindrome `>x` in base `b`
    -   Closed form of nth basewise palindrome
    -   Closed form of getting the index of a palindrome in some base (returns `Nothing` if it is non-palindromic)
    -   Checking whether a number is a palindrome in some base
    -   Computing all palindromic bases of some number
-   Factors, uses prime factorisations to compute factor lists or integer factorisations of numbers.
-   Integer partition, constructs a tree describing all ways to sum to `n` (values are strictly decreasing as you go deeper into the tree).
-   Composites, has an infinite list of composites, non-primes (composes and 1), composites of some residue class modulo some n, and an infinite list of highly composite numbers (and Ramanujan's largely composite numbers).
-   Multiset, computes subsets and partitions of a multiset, I used this for computing non-prime factorisations.
-   Raise to power modulo, modular exponentation, very simple.

## Utility Oriented Programs

-   Digits, extracts digits in some base of an integral (used for the palindromes).
-   Binary display, I like this one, shows the binary expansion of a (possibly infinite) list using block unicodes (it uses ` `, `▀`, `▄` and `█` with ANSI foreground and background colours). It shows two numbers on one row (so bits have an aspect ratio of 1:1).
-   Scripted numbers, produces superscript and subscript strings of integers.

# Some Visual Computations

## Binary hamming weight

Binary display of numbers with a binary hamming weight of two! So fractal!

```
     ▄▀█
    ▄▀▀▄
    █▄▀
   █  ▄▀
   █▄▀
  █   ▄▀
  █ ▄▀
 ▄▀▀   ▄
 █   ▄▀
 █ ▄▀
▄▀▀    ▄
█    ▄▀
█  ▄▀
█▄▀
```

## Digits of π

```
1415926535 8979323846 2643383279 5028841971 6939937510
5820974944 5923078164 0628620899 8628034825 3421170679
8214808651 3282306647 0938446095 5058223172 5359408128
4811174502 8410270193 8521105559 6446229489 5493038196
4428810975 6659334461 2847564823 3786783165 2712019091
4564856692 3460348610 4543266482 1339360726 0249141273
7245870066 0631558817 4881520920 9628292540 9171536436
7892590360 0113305305 4882046652 1384146951 9415116094
3305727036 5759591953 0921861173 8193261179 3105118548
0744623799 6274956735 1885752724 8912279381 8301194912
9833673362 4406566430 8602139494 6395224737 1907021798
6094370277 0539217176 2931767523 8467481846 7669405132
0005681271 4526356082 7785771342 7577896091 7363717872
1468440901 2249534301 4654958537 1050792279 6892589235
4201995611 2129021960 8640344181 5981362977 4771309960
5187072113 4999999837 2978049951 0597317328 1609631859
5024459455 3469083026 4252230825 3344685035 2619311881
7101000313 7838752886 5875332083 8142061717 7669147303
5982534904 2875546873 1159562863 8823537875 9375195778
1857780532 1712268066 1300192787 6611195909 2164201989
```

## Digits of √2

```
4142135623 7309504880 1688724209 6980785696 7187537694
8073176679 7379907324 7846210703 8850387534 3276415727
3501384623 0912297024 9248360558 5073721264 4121497099
9358314132 2266592750 5592755799 9505011527 8206057147
0109559971 6059702745 3459686201 4728517418 6408891986
0955232923 0484308714 3214508397 6260362799 5251407989
6872533965 4633180882 9640620615 2583523950 5474575028
7759961729 8355752203 3753185701 1354374603 4084988471
6038689997 0699004815 0305440277 9031645424 7823068492
9369186215 8057846311 1596668713 0130156185 6898723723
5288509264 8612494977 1542183342 0428568606 0146824720
7714358548 7415565706 9677653720 2264854470 1585880162
0758474922 6572260020 8558446652 1458398893 9443709265
9180031138 8246468157 0826301005 9485870400 3186480342
1948972782 9064104507 2636881313 7398552561 1732204024
5091227700 2269411275 7362728049 5738108967 5040183698
6836845072 5799364729 0607629969 4138047565 4823728997
1803268024 7442062926 9124859052 1810044598 4215059112
0249441341 7285314781 0580360337 1077309182 8693147101
7111168391 6581726889 4197587165 8215212822 9518488472
```

# Highly palindromic numbers

This can compute more, don't want to flood. A lot of these numbers are largely composite too!

```
3 = 11₂
5 = 101₂ 11₄
10 = 101₃ 22₄ 11₉
21 = 10101₂ 111₄ 33₆ 11₂₀
36 = 121₅ 44₈ 33₁₁ 22₁₇ 11₃₅
60 = 66₉ 55₁₁ 44₁₄ 33₁₉ 22₂₉ 11₅₉
80 = 2222₃ 212₆ 88₉ 55₁₅ 44₁₉ 22₃₉ 11₇₉
120 = AA₁₁ 88₁₄ 66₁₉ 55₂₃ 44₂₉ 33₃₉ 22₅₉ 11₁₁₉
180 = CC₁₄ AA₁₇ 99₁₉ 66₂₉ 55₃₅ 44₄₄ 33₅₉ 22₈₉ 11₁₇₉
252 = 2002₅ 252₁₀ EE₁₇ CC₂₀ 99₂₇ 77₃₅ 66₄₁ 44₆₂ 33₈₃ 22₁₂₅ 11₂₅₁
300 = 606₇ 454₈ 363₉ 1A1₁₃ FF₁₉ CC₂₄ AA₂₉ 66₄₉ 55₅₉ 44₇₄ 33₉₉ 22₁₄₉ 11₂₉₉
720 = 5A5₁₁ OO₂₉ KK₃₅ II₃₉ GG₄₄ FF₄₇ CC₅₉ AA₇₁ 99₇₉ 88₈₉ 66₁₁₉ 55₁₄₃ 44₁₇₉ 33₂₃₉ 22₃₅₉ 11₇₁₉
1080 = 252₂₂ UU₃₅ RR₃₉ OO₄₄ KK₅₃ II₅₉ FF₇₁ CC₈₉ AA₁₀₇ 99₁₁₉ 88₁₃₄ 66₁₇₉ 55₂₁₅ 44₂₆₉ 33₃₅₉ 22₅₃₉ 11₁₀₇₉
1440 = aa₃₉ WW₄₄ UU₄₇ OO₅₉ KK₇₁ II₇₉ GG₈₉ FF₉₅ CC₁₁₉ AA₁₄₃ 99₁₅₉ 88₁₇₉ 66₂₃₉ 55₂₈₇ 44₃₅₉ 33₄₇₉ 22₇₁₉ 11₁₄₃₉
1680 = ee₄₁ ZZ₄₇ UU₅₅ SS₅₉ OO₆₉ LL₇₉ KK₈₃ GG₁₀₄ FF₁₁₁ EE₁₁₉ CC₁₃₉ AA₁₆₇ 88₂₀₉ 77₂₃₉ 66₂₇₉ 55₃₃₅ 44₄₁₉ 33₅₅₉ 22₈₃₉ 11₁₆₇₉
2160 = 4A4₂₂ jj₄₇ ee₅₃ aa₅₉ UU₇₁ RR₇₉ OO₈₉ KK₁₀₇ II₁₁₉ GG₁₃₄ FF₁₄₃ CC₁₇₉ AA₂₁₅ 99₂₃₉ 88₂₆₉ 66₃₅₉ 55₄₃₁ 44₅₃₉ 33₇₁₉ 22₁₀₇₉ 11₂₁₅₉
```
