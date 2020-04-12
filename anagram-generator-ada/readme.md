# Tests

- `randomInt()` range test

- `isWord()` true test

- `isWord()` false test

- `isWord()` escape sequence test

- `scrambleWord()` anagram test

- `scrambleWord()` derangement test

- `processText()` manual analysis

## Considerations

- Your function definitions might not match! In which case this harness
  is probably worthless.

- If the automatic tests freeze for some reason (especially after 
  scrambleWord(hel)), there is a good chance you messed up either your
  loop bounds or your randomInt bounds and caused an infinite loop.

- randomInt() is treated as [inclusive, exclusive). Meaning if you call
  randomInt(5, 10), the range of possible values is {5, 6, 7, 8, 9}.

## Assertion Failures

```"randomInt(M, N) failed! Got O"```

  This is when the randomInt() test fails. Specifically, this error
  occurs when a value outside the range [M, N) is returned. This is
  usually when you mess up a bound or fail to Reset() your generator.
  However, this could also just be because you defined your function
  differently.

-----------------------------------------------------------------------

```"isWord(xxxx) failed! Expected FALSE (or TRUE)"```

  This is when the isWord() test fails, either when a non-alphabetic
  word is considered to be alphabetic by the function (and returns
  true) or when an alphabetic word is considered not alphabetic (and
  returns false).

-----------------------------------------------------------------------

```"scrambleWord(xxxx) failed and returned yyyy. Words less than 4 characters must be untouched!"```

  This error is returned when scrambleWord() accidentally modifies
  a word that is less than 4 characters. 3-letter words do not have
  any possible permutations because there is only a single letter. As
  such, any modification would be erroneous. As would any modification
  on 2- and 1-letter words. I recommend adding an if-condition to check
  the length of a word before operating on it in scrambleWord(), though
  most loops with correct bounds won't need this, so this error is
  usually indicative of a bigger problem.

-----------------------------------------------------------------------

```"isWord(xxxx) is not a word!"```

  This check occurs in the scrambleWord() test as well. In some cases,
  an implementation of this function might accidentally write the wrong
  character. In which case if it is non-alphabetic, we catch it here.
  Don't worry, we check for erroneous alphabetic characters in a bit.

-----------------------------------------------------------------------

```"xxxx != yyyy"```

  Another smoke test like the above, this one checks the length of the
  two words. I felt like this error message was succinct enough such
  that if the words are not the same length, it's pretty obvious that
  they are not anagrams of each other.

-----------------------------------------------------------------------

```"scrambleWord(xxxx) => yyyy failed. First letters don't match!"```

  The constraint of this assignment is that the first and last letters
  of the before and after transposition strings must match.

-----------------------------------------------------------------------

```"scrambleWord(xxxx) => yyyy failed. Last letters don't match!"```

  Same as above, but with last letters.

-----------------------------------------------------------------------
```"xxxx and yyyy are not anagrams"```

  I would say that along with the first/last letter checks, this is
  the main check of the function. Words that manage to pass previous
  smoke checks can still fail the function, and if they do, it is
  usually from this check. A bit more complex than the rest, this test
  checks the composition of the words independent of permutation, thus
  reducing this from a permutation problem to a combination problem,
  doing so by means of "checking off" letters in a separate temporary
  word. Unfortunately, failing this test will mean a looot of debugging.

-----------------------------------------------------------------------
