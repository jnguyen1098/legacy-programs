with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Calendar; use Ada.Calendar;
with Ada.Directories; use Ada.Directories;

procedure Wordscram is

-------------------------- Main Subprograms ---------------------------

    procedure getFilename(File_Name : out String; Len : out Integer);
    function processText(File_Name : String) return Integer;
    procedure scrambleWord(Str : in out String);
    function randomInt(A : Integer; B : Integer) return Integer;
    function isWord(Str : String) return Boolean;

-----------------------------------------------------------------------

    -- Verifies a filename and returns it to main
    procedure getFilename(File_Name : out String; Len : out Integer) is
    begin
        loop
            -- Prompt user for filename
            Put("File name to open: ");
            Get_Line(File_Name, Len);

            -- Filename testing. Assumed to ask again if loop doesn't break
            if (File_Name(File_Name'First .. Len) = "" or else
                File_Name(File_Name'First .. File_Name'First) = "."  or else
                File_Name(File_Name'First .. File_Name'First) = "/" or else
                exists(File_Name(File_Name'First .. Len)) = False) then
                Put_Line("Could not open file! Re-try.");
                New_Line;

            -- Loop will break and function will exit assuming all tests pass
            else exit;
            end if;
        end loop;
    end getFilename;

    -- Processes the words within a file
    function processText(File_Name : String) return Integer is
    Word_Count : Integer := 0;
            Fp : Ada.Text_IO.File_Type;
    begin
        -- Open the file for playback
        Open(File => Fp,
             Mode => In_File,
             Name => File_Name);

        -- Print every original line
        Put_Line("                      O r i g i n a l      T e x t");
        Put_Line("--------------------------------------------------" &
                 "----------------------");
        while not End_Of_File(Fp) loop
            Put_Line(Get_Line(Fp));
        end loop;
        New_Line;
        Close(Fp);

        -- Attempt to open the file again, this time for processing
        Open(File => Fp,
             Mode => In_File,
             Name => File_Name);

        -- Process file by iterating over every line
        Put_Line("                      T r a n s p o s e d  T e x t");
        Put_Line("--------------------------------------------------" &
                 "----------------------");

        -- Like above, we iterate over the file
        while not End_Of_File(Fp) loop

        declare
            -- Left and Right represent slice/substring indices for parsing
            -- Line is the fixed string holding the line. Because it is within
            -- block scope with the loop, we do not have to use unbounded
            -- string. This is because we re-create it every loop iteration.
             Left : Integer := 1;
            Right : Integer := 1;
             Line : String := Get_Line(Fp);

        begin
            -- Parsing algorithm that aims to 'greedily' select the largest
            -- word using Left and Right to index potential substrings, and
            -- then moving to the next potential candidate.

            -- This algorithm will backtrack upon discovering a non-alpha
            -- character and use the backtracked 'word' as the scramble target.
            -- Otherwise, it will just print out, one-by-one, as many non-
            -- alpha characters as possible before returning to the greedy
            -- word building algorithm as mentioned earlier.

            -- This algorithm, as shown in this while loop, will only run for
            -- the length of the current line. This is for bounds-checking.
            while Left <= Line'Length and then Right <= Line'Length loop

                -- If current substring is a word...
                if isWord(Line(Left .. Right)) then

                    -- ...then greedily grow it until it is no longer a word.
                    while Right <= Line'Length and then
                    isWord(Line(Left .. Right)) loop
                        Right := Right + 1;
                    end loop;

                    -- Upon finding a non-alpha character, we backtrack 1
                    Right := Right - 1;

                    -- We know that this backtracked word is the largest
                    -- possible substring that satisfies isWord(), so we go on
                    -- to scramble it in-place and then print it.
                    scrambleWord(Line(Left .. Right));
                    Put(Line(Left .. Right));

                    -- In doing so, we increment the word count.
                    Word_Count := Word_Count + 1;

                    -- Then, we set both Left and Right indices into the first
                    -- character following the word we just scrambled.
                    Right := Right + 1;
                    Left := Right;

                -- Otherwise, if the current substring is NOT a word...
                else
                    
                    -- ...we don't want to do anything else but print it.
                    Put(Line(Left .. Right));

                    -- In fact, we print AS MANY non-alphabetic characters as
                    -- possible. As long as there are more, it will always
                    -- fall back to this else statement.
                    Left := Left + 1;
                    Right := Right + 1;

                end if;

            end loop; -- We are finished parsing the file

            New_Line; -- Putting a new line for cleanliness

        end; -- End loop scope

        end loop; -- End the actual loop

        -- Close the file when we are done and return word count
        Close(Fp);
        return Word_Count;

    end processText;

    -- Scramble a string / "word" in-place
    procedure scrambleWord(Str : in out String) is
        Copy : String := Str(Str'First + 1 .. Str'Last - 1);
        Rand : Integer;
    begin
        -- Only words 4 char. or greater are eligible
        if Str'Length > 3 then
            -- Iterate over the middle characters
            for i in 2 .. Str'Length - 1 loop
                -- Keep looping until we get a unique letter
                loop
                    Rand := randomInt(Copy'First, Copy'Last + 1);
                    exit when Copy(Rand) /= '.';
                end loop;
                -- Copy the character over
                Str(Str'First + i - 1) := Copy(Rand);
                -- Mark the original character spot as "used"
                Copy(Rand) := '.';
            end loop;
        end if;

    end scrambleWord;

    -- Check if a string is completely alphabetic
    function isWord(Str : String) return Boolean is
    begin
        -- Check for empty string
        if Str = "" then
            return False;
        end if;

        -- Check if each character is alphabetic
        for i in Str'First .. Str'Last loop
            if not Is_Letter(Str(i)) then
                return False;
            end if;
        end loop;

        -- If we reach this point, it's a good string and we exit
        return true;
    end isWord;

    -- Generate a random number on the interval [A, B) (incl. - excl.)
    function randomInt(A : Integer; B : Integer) return Integer is
        subtype IntGen is Integer range A .. B - 1;
        package RandGen is new Ada.Numerics.Discrete_Random (IntGen);
        use RandGen;
        RandIntGen : Generator;
    begin
        -- Generate our number
        Reset(RandIntGen);
        return Random(RandIntGen);
    end randomInt;

------------------------- Testing Subprograms -------------------------

    procedure Test_randomInt(A : Integer; B : Integer);
    procedure Test_isWord(Str : String; Expected : Boolean);
    procedure Test_scrambleWord(Str : String);
    procedure Test_derangement(Str : String);

-----------------------------------------------------------------------

    -- Tests the return value of randomInt()
    procedure Test_randomInt(A : Integer; B : Integer) is
    Rand_Value : Integer;
        Checks : array(-10000 .. 10000) of Integer := (others => 0);
    begin
        -- Assert all outputs are in range for 10,000 trials
        for i in 1 .. 10000 loop
            Rand_Value := randomInt(A, B);
            Checks(Rand_Value) := 1;
            Assert(Rand_Value >= A and Rand_Value < B, "randomInt(" &
                    Integer'Image(A) & "," & Integer'Image(B) &
                    ") failed!" & " Got " & Integer'Image(Rand_Value));
        end loop;
        -- Assert function is surjective / "onto" range
        for i in A .. B - 1 loop
            Assert(Checks(i) = 1, "Value " & Integer'Image(i) &
                   " not found in range [" & Integer'Image(A) & ", " &
                    Integer'Image(B) & ").");
        end loop;
        Put_Line("    PASS: randomInt(" & Integer'Image(A) & "," &
                 Integer'Image(B) & ")");
    end Test_randomInt;

    -- Tests the return value of isWord()
    procedure Test_isWord(Str : String; Expected : Boolean) is
    begin
        -- Assert word status
        Assert(isWord(Str) = Expected, "isWord(" & str & ") failed! " &
                "Expected " & Boolean'Image(Expected));
        Put_Line("    PASS: isWord(" & str & ")");
    end test_isWord;

    -- Tests the word scrambling of scrambleWord()
    procedure Test_scrambleWord(Str : String) is
    Copy : String := Str;
    Temp : String := Str;
    begin
        -- Scramble a copy of the word
        scrambleWord(Copy);

        -- Assert that words with length < 4 are untouched
        if Str'Length < 4 then
            Assert(Str = Copy, "scrambleWord(" & Str & ") failed" &
                   " and returned " & Copy & ". Words less than" &
                   " 4 characters must be untouched!");
        end if;

        -- Assert that scrambleWord() creates an actual word
        Assert(isWord(Copy) = True, Copy & " is not a word!");
        
        -- Assert the two strings are the same length
        Assert(Copy'Length = Str'Length, Copy & " != " & Str);

        -- Assert that the first letters match
        Assert(Copy(Copy'First) = Str(Str'First), 
                "scrambleWord(" & Str & ") => " & Copy &
                " failed. First letters don't match!");

        -- Assert that the last letters match
        Assert(Copy(Copy'Last) = Str(Str'Last),
                "scrambleWord(" & Str & ") => " & Copy &
                " failed. Last letters don't match!");

        -- Confirm that the two words are anagrams by checking letters
        for i in Copy'First .. Copy'Last loop
            for j in Temp'First .. Temp'Last loop
                if Copy(i) = Temp(j) then
                    Temp(j) := '.';
                end if;
            end loop;
        end loop;

        -- Verify the guard character, the period. Because we verify
        -- that isWord() gives true, the period is free to use
        for i in Temp'First .. Temp'Last loop
            Assert(Temp(i) = '.', Copy & " and " & Str &
                   " are not anagrams!");
        end loop;

        -- Test passed at this point
        Put_Line("    PASS: scrambleWord(" & Str & ")");

    end Test_scrambleWord;

    -- Tests substring derangement. HELLO has a potential derangement
    -- at ELL. The first and last character must remain, but all of the
    -- characters in-between must be moved. Of course, sometimes they,
    -- by complete chance, don't. Which is why I will run 1000 trials.
    --
    -- The goal is to find at least one derangement, which is where all
    -- of the non-first non-last characters are out of order.
    -- 
    -- This test will run after all the Test_scrambleWord() trials so
    -- that I know scrambleWord() is capable of creating anagrams.
    procedure Test_derangement(Str : String) is
    Copy : String := Str;
    Found : Boolean := True;
    begin
        if Str'Length > 3 then
            for trial in 1 .. 1000 loop
                Found := True;
                scrambleWord(Copy);

                for i in Copy'First + 1 .. Copy'Last - 1 loop
                    if Copy(i) = Str(i) then
                        Found := False;
                    end if;
                end loop;

                if Found then
                    exit;
                end if;

            end loop;
            Assert(Found, "Could not force derangement on " &
                   "scrambleWord(" & Str & "). Returned " & Copy);

            Put_Line("    PASS: scrambleWord(" & Str & ")");
        end if;
    end Test_derangement;

    -- Main variables
    File_Name_Len : Integer;
        File_Name : String(1..5000);
        Num_Words : Integer;

    Start_Time, End_Time : Time;
           Millis : Duration;

-----------------------------------------------------------------------

begin
    -- Test harness
    Put_Line(ESC & "[32m" & "Starting automatic tests!" & ESC & "[0m");

    Start_Time := Clock;

    -- randomInt() range test
    Put_Line("Testing randomInt()...");
    New_Line;
        Test_randomInt(1, 5);
        Test_randomInt(10, 500);
        Test_randomInt(-10, 5);
        Test_randomInt(-10, 0);
        Test_randomInt(0, 50);
        Test_randomInt(-100, 100);
        Test_randomInt(-1, 0);
        Test_randomInt(1, 2);
        Test_randomInt(2, 3);
        Test_randomInt(10, 11);
        Test_randomInt(100, 101);
        Test_randomInt(1000, 1001);
    New_Line;

    -- isWord() true test
    Put_Line("Testing isWord() for true...");
    New_Line;
        Test_isWord("a", true);
        Test_isWord("z", true);
        Test_isWord("abasjdklflksdf", true);
        Test_isWord("A", true);
        Test_isWord("Z", true);
        Test_isWord("AZ", true);
        Test_isWord("ASDFJSADFJSADKFJAG", true);
        Test_isWord("ABCDEFGHIJKLMNOPQRSTUVWXYZ", true);
        Test_isWord("abcdefghijklmnopqrstuvwxys", true);
        
        for i in 65 .. 90 loop
            Test_isWord(Character'Image(Character'Val(i))(2 .. 2), true);
        end loop;

        for i in 97 .. 122 loop
            Test_isWord(Character'Image(Character'Val(i))(2 .. 2), true);
        end loop;
    New_Line;

    -- isWord() false test
    Put_Line("Testing isWord() for false...");
    New_Line;
        Test_isWord("", false);
        Test_isWord("1", false);
        Test_isWord("28375498275", false);
        Test_isWord("!", false);
        Test_isWord(" ", false);
        Test_isWord("ABa313", false);
        Test_isWord("aaskdfjaskdfashdf1", false);
        Test_isWord("hsadfjasdf[aksdjfaskdf", false);
        Test_isWord("sakdjfl`aksdjf", false);
        Test_isWord("adsf~~asdfasdf", false);
        Test_isWord("ABCDEFGHIJKL~MNOPQRSTUVWXYZ", false);
        Test_isWord("1234567890][';/.[p,p.][}{>{}>}{>{}", false);

        for i in 32 .. 64 loop
            Test_isWord(Character'Image(Character'Val(i))(2 .. 2), false);
        end loop;

        for i in 91 .. 96 loop
            Test_isWord(Character'Image(Character'Val(i))(2 .. 2), false);
        end loop;

        for i in 123 .. 126 loop
            Test_isWord(Character'Image(Character'Val(i))(2 .. 2), false);
        end loop;
    New_Line;

    -- isWord() escape sequence test
    Put_Line("Testing isWord() for false with escape sequences...");
    New_Line;
        for i in 0 .. 31 loop
            Test_isWord(ESC & Integer'Image(i), false);
        end loop;

        Test_isWord(ESC & Integer'Image(127), false);
    New_Line;

    -- scrambleWord() anagram test
    Put_Line("Testing scrambleWord()...");
    New_Line;
        Test_scrambleWord("a");
        Test_scrambleWord("z");
        Test_scrambleWord("ad");
        Test_scrambleWord("hel");
        Test_scrambleWord("HASfdhasFDDFhasdf");
        Test_scrambleWord("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        Test_scrambleWord("abcdefghijklmnopqrstuvwxyz");
        Test_scrambleWord("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
        Test_scrambleWord("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB");
        Test_scrambleWord("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ");
        Test_scrambleWord("OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO");
        Test_scrambleWord("IJUSTLOSTTHEGAMEEEEEEEEEEEEEEEEEEEEEEEEEE");
        Test_scrambleWord("ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                          "abcdefghijklmnopqrstuvwxyz");
        Test_scrambleWord("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmno" &
                          "pqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcd" &
                          "efghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRS" &
                          "TUVWXYZabcdefghijklmnopqrstuvwxyzABCDEFGH" &
                          "IJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvw" &
                          "xyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijkl" &
                          "mnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZa" &
                          "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOP" &
                          "QRSTUVWXYZabcdefghijklmnopqrstuvwxyzABCDE" &
                          "FGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrst" &
                          "uvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghi" &
                          "jklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWX" &
                          "YZabcdefABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFG");
        New_Line;

    -- scrambleWord() derangement test
    Put_Line("Testing derangements...");
    New_Line;
        Test_derangement("ABCDEFGH");
        Test_derangement("abcdefgh");
        Test_derangement("IJKLMNOP");
        Test_derangement("ijklmnop");
        Test_derangement("QRSTUVWX");
        Test_derangement("qrstuvwx");
        Test_derangement("YZ");
        Test_derangement("yz");
    New_Line;


    Put_Line(ESC & "[32m" & "Passed all automatic tests" & ESC &"[0m");
    Put_Line(ESC & "[32m" & "Starting manual tests!" & ESC & "[0m");

    -- processTest() manual analysis
    New_Line;
    Put_Line("Testing 1000 most common English words...");
    Put_Line("You should be able to recognize these");
    New_Line;
    Num_Words := processText("test/test.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing punctuation nightmare");
    Put_Line("Punctuation should be intact; words scrambled");
    New_Line;
    Num_Words := processText("test/punctuation.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing 3-letter words");
    Put_Line("There should be NO change");
    New_Line;
    Num_Words := processText("test/3letter.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing 2-letter words");
    Put_Line("There should be NO change");
    New_Line;
    Num_Words := processText("test/2letter.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing 1-letter words");
    Put_Line("There should be NO change");
    New_Line;
    Num_Words := processText("test/1letter.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing d2.txt");
    New_Line;
    Num_Words := processText("test/d2.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing dijkstra.txt");
    New_Line;
    Num_Words := processText("test/dijkstra.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing middleE.txt");
    New_Line;
    Num_Words := processText("test/middleE.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing z.txt");
    New_Line;
    Num_Words := processText("test/z.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing empire.txt");
    New_Line;
    Num_Words := processText("test/empire.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Testing quotations.txt");
    New_Line;
    Num_Words := processText("test/quotations.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    New_Line;
    Put_Line("Arthur's behemoth");
    New_Line;
    -- Num_Words := processText("test/amurica.txt");
    New_Line;
    Put_Line("Word Count: " & Integer'Image(Num_Words));

    End_Time := Clock;
    Millis := (End_Time - Start_Time) * 1000;

    New_Line;
    Put_Line(ESC & "[32m" & "All automatic tests passed!" & ESC & "[0m");
    Put_Line("Now, scroll up and manually examine your sample outputs.");
    New_Line;

    Put_Line(ESC & "[32m" & "Runtime: " & Duration'Image(Millis) & ESC & "[0m");

    -- Main code

    getFilename(File_Name, File_Name_Len);
    New_Line;
    Num_Words := processText(File_Name(1..File_Name_Len));
    New_Line;
    Put_Line("Word count: " & Integer'Image(Num_Words));

end Wordscram;

-----------------------------------------------------------------------
