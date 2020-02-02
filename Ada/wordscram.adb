with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Numerics.Discrete_Random;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure Wordscram is

-------------------------- Main Subprograms ---------------------------

    procedure getFilename(File_Name : out String; Len : out Integer);
    function processText(File_Name : String) return Integer;
    procedure scrambleWord(Str : in out String; Len : in Integer);
    function randomInt(A : Integer; B : Integer) return Integer;
    function isWord(Str : String) return Boolean;

    function Does_File_Exist(Name : String) return Boolean;

-----------------------------------------------------------------------

    -- Verifies a filename and returns it to main
    procedure getFilename(File_Name : out String; Len : out Integer) is
    begin
        loop
            Put("File name to open: ");
            Get_Line(File_Name, Len);
            if (Does_File_Exist(File_Name(File_Name'First .. Len))
                    = False) then
                Put_Line("Could not open file! Re-try.");
            else exit;
            end if;
        end loop;
    end getFilename;

    -- Helper function to verify if file exists
    function Does_File_Exist(Name : String) return Boolean is
        Fp : Ada.Text_IO.File_Type;
    begin
        -- Asking for forgiveness . . . 
        Open(Fp, In_File, Name);
        Close(Fp);
        return True;

    exception
        -- . . . rather than permission. :-)
        when Name_Error =>
            return False;
    end Does_File_Exist;

    -- Processes the words within a file
    function processText(File_Name : String) return Integer is
    Word_Count : Integer := 0;
            Fp : Ada.Text_IO.File_Type;
    begin
        -- Attempt to open the file
        Open(File => Fp,
             Mode => In_File,
             Name => File_Name);

        -- Process every line
        while not End_Of_File(Fp) loop
        declare
             Left : Integer := 1;
            Right : Integer := 1;
             Line : String := Get_Line(Fp);
        begin

            while Left <= Line'Length and then Right <= Line'Length loop
                if isWord(Line(Left .. Right)) = False then
                    Put(Line(Left .. Right));
                    Left := Left + 1;
                    Right := Right + 1;
                else
                    while Right <= Line'Length and then isWord(Line(Left .. Right)) loop
                        Right := Right + 1;
                    end loop;
                    Right := Right - 1;
                    scrambleWord(Line(Left .. Right), Right - Left + 1);

                    Word_Count := Word_Count + 1;

                    Right := Right + 1;
                    Left := Right;

                end if;
            end loop;

            Put_Line("");

        end;
        end loop;

        -- Close the file when we are done and return word count
        Close(Fp);
        return Word_Count;

    end processText;

    -- Scramble a string / "word" in-place
    procedure scrambleWord(Str : in out String; Len : in Integer) is
        Copy : String := Str(Str'first + 1 .. Str'first + Len - 2);
        Rand : Integer;
    begin

        if Len > 3 then
            for i in 2 .. Len - 1 loop

                Rand := randomInt(Copy'First, Copy'Last + 1);

                for l in 1 .. 1000 loop
                if Copy(Rand) = '.' then
                    Rand := randomInt(Copy'First, Copy'Last + 1);
                end if;
                end loop;

                Str(Str'first + i - 1) := Copy(Rand);
                Copy(Rand) := '.';

            end loop;

        end if;

        Put(Str);

    end scrambleWord;

    -- Check if a string is completely alphabetic
    function isWord(Str : String) return Boolean is
    Ascii_Value : Integer;
    begin
        -- Check for empty string
        if Str = "" then
            return false;
        end if;

        -- Check if each character is alpha
        for i in 1 .. Str'Length loop
            Ascii_Value := Character'Pos(Str(Str'first + i - 1));
            if Ascii_Value < 65 or Ascii_Value > 122 then
                return false;
            elsif Ascii_Value > 90 and Ascii_Value < 97 then
                return false;
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

    procedure Test_isWord(Str : String; Expected : Boolean);
    procedure Test_randomInt(A : Integer; B : Integer);

-----------------------------------------------------------------------

    -- Tests the return value of isWord()
    procedure Test_isWord(Str : String; Expected : Boolean) is
    begin
        -- Assert word status
        Assert(isWord(Str) = Expected, "isWord(" & str & ") failed! " &
                "Expected " & Boolean'Image(Expected));
        Put_Line("    PASS: isWord(" & str & ")");
    end test_isWord;

    -- Tests the return value of randomInt()
    procedure Test_randomInt(A : Integer; B : Integer) is
    Rand_Value : Integer;
        Checks : array(-10000 .. 10000) of Integer := (others => 0);
    begin
        -- Assert all outputs are in range for 10,000 trials
        for i in 1 .. 10000 loop
            Rand_Value := randomInt(A, B);
            Checks(Rand_Value) := 1;
            Assert(Rand_Value >= A and Rand_Value < B,
                    "randomInt(" & Integer'Image(A) & "," & Integer'Image(B) & ") failed!"
                    & " Got " & Integer'Image(Rand_Value));
        end loop;
        -- Assert function is surjective / "onto" range
        for i in A .. B - 1 loop
            Assert(Checks(i) = 1, "Value " & Integer'Image(i) & " not found in range ["
                    & Integer'Image(A) & ", " & Integer'Image(B) & ").");
        end loop;
        Put_Line("    PASS: randomInt(" & Integer'Image(A) & "," & Integer'Image(B) & ")");
    end Test_randomInt;

    -- Tests the word scrambling of scrambleWord()
    

    -- Testing variables

    -- Main variables
    File_Name_Len : Integer;
        File_Name : String(1..100);
        Num_Words : Integer;

-----------------------------------------------------------------------

begin

    -- Test harness
    Put_Line("Testing isWord() for true...");
    New_Line;
        Test_isWord("a", true);
        Test_isWord("z", true);
        Test_isWord("abasjdklflksdf", true);
        Test_isWord("A", true);
        Test_isWord("Z", true);
        Test_isWord("AZ", true);
        Test_isword("ASDFJSADFJSADKFJAG", true);
        Test_isword("ABCDEFGHIJKLMNOPQRSTUVWXYZ", true);
        Test_isword("abcdefghijklmnopqrstuvwxys", true);
    New_Line;

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
    New_Line;

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

    Put_Line("All tests passed!");
    
    getFilename(File_Name, File_Name_Len);
    Put_Line(File_Name(1..File_Name_Len));
    Num_Words := processText(File_Name(1..File_Name_Len));
    Put_Line("Word count: " & Integer'Image(Num_Words));

    -- Main code


end Wordscram;

-----------------------------------------------------------------------
