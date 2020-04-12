# Boomer Programs
Here you'll find an assortment of random programs written in older legacy programming languages such as Ada, COBOL, and Fortran.

You may run all of these projects at once by simply typing `make` at the root, or perhaps run alternate versions of these programs by using each folder's dedicated `makefile`s. In making each of these projects, I was also instructed to write a reflection report.

## Lucifer (Fortran)
This is a Feistel round cryptographic cipher that would later become the predecessor to the well known Data Encryption Standard (DES) algorithm used today. 

## Scrambler (Ada)
This program aims to simulate the well-known urban legend about how we perceive words. By entering a filename to a text file, this program aims to output the same sort of text "jumbling" effect as depicted by this passage:

```
Aoccdrnig to a rscheearch at Cmabrigde Uinervtisy, it deosn't mttaer in waht oredr the ltteers in a wrod are, the olny iprmoetnt tihng is taht the frist and lsat ltteer be at the rghit pclae. The rset can be a toatl mses and you can sitll raed it wouthit porbelm. Tihs is bcuseae the huamn mnid deos not raed ervey lteter by istlef, but the wrod as a wlohe.
```

## Babylonian Square Root (COBOL)
An iterative improvement algorithm, this very old mathematical algorithm is known as a _divide-and-average_ approach to determining square roots. It was also an experiment with using ANSI CSI terminal escape codes for outputting. I think I'll stick to `ncurses`.

## Russian Peasant Multiplication (Ada, C, Fortran)
Written in three languages using both recursive and iterative implementations, these programs aim to benchmark the performance between said languages. However, when compiled with `-O3` or `-Ofast`, the differences between recursion and iteration are nonexistent, even without using a tail-recursive implementation. See the reflection pdf for more details.