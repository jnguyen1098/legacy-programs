-----------------------------------------------------------------------
--                          Word Scrambler                           --
--                    By: Jason Nguyen (XXXXXXX)                     --
-----------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;

procedure Scramble is

-------------------------- Main Subprograms ---------------------------

    procedure   getFilename(File_Name : out String; Len : out Integer);
    function    processText(File_Name : String) return Integer;
    procedure   scrambleWord(Str : in out String);
    function    randomInt(A : Integer; B : Integer) return Integer;
    function    isWord(Str : String) return Boolean;

-----------------------------------------------------------------------

    -- Verifies a filename and returns it to main
    procedure getFilename(File_Name : out String; Len : out Integer) is
    begin
        File_Name_Check: loop
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
            else exit File_Name_Check;
            end if;
        end loop File_Name_Check;
    end getFilename;

-----------------------------------------------------------------------

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

-----------------------------------------------------------------------

    -- Scramble a string / "word" in-place
    procedure scrambleWord(Str : in out String) is
        -- Copies the string not including first/last char.
        Copy : String := Str(Str'First + 1 .. Str'Last - 1);
        Rand : Integer;
    begin
        -- Only words 4 char. or greater will be scrambled
        if Str'Length > 3 then
            -- Iterate over the middle characters
            for i in 2 .. Str'Length - 1 loop
                -- Keep looping until we get a unique letter
                loop
                    Rand := randomInt(A => Copy'First, B => Copy'Last + 1);
                    exit when Copy(Rand) /= '.';
                end loop;
                -- Copy this character over to the scrambled word
                Str(Str'First + i - 1) := Copy(Rand);
                -- Mark the original character spot as "used".
                -- This method only works because only true words
                -- are passed to scrambleWord().
                Copy(Rand) := '.';
            end loop;
        end if;

    end scrambleWord;

-----------------------------------------------------------------------

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

-----------------------------------------------------------------------

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

------------------------------ Variables ------------------------------

    File_Name_Len : Integer;
        File_Name : String(1..5000);
        Num_Words : Integer;

-------------------------------- Main ---------------------------------

begin

    getFilename(File_Name, File_Name_Len);
    New_Line;
    Num_Words := processText(File_Name(1..File_Name_Len));
    New_Line;
    Put_Line("Word count: " & Integer'Image(Num_Words));

end Scramble;

-----------------------------------------------------------------------
