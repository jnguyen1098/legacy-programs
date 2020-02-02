with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Numerics.Discrete_Random;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure Wordscram is

-------------------------- Main Subprograms ---------------------------

    -- Verifies a filename and returns it to main
    procedure getFilename (File_Name : out String; Len : out Integer) is
    begin
        Put("File name to open: ");
        Get_Line(File_Name, Len);
        
    end getFilename;

    -- Processes the words within a file


    -- Scramble a string / "word" in-place
    procedure scrambleWord(Str : in out String; Len : in Integer) is
        
    begin
        for i in 1 .. Len loop
            Str(i) := 'A';
        end loop;
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
        for i in Str'range loop
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
    Put(File_Name(1..File_Name_Len));

    -- Main code


end Wordscram;

-----------------------------------------------------------------------
