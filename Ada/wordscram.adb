with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Numerics.Discrete_Random;

procedure Wordscram is

--------------------------- Main Functions ----------------------------

    -- Check if a string is completely alphabetic
    function isWord(str : String) return Boolean is
    Ascii_Value : Integer;
    begin
        -- Check for empty string
        if str = "" then
            return false;
        end if;

        -- Check if each character is alpha
        for i in str'range loop
            Ascii_Value := Character'Pos(str(str'first + i - 1));
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
        Reset(RandIntGen);
        return Random(RandIntGen);
    end randomInt;

-------------------------- Testing Functions --------------------------

    -- Tests the return value of isWord()
    procedure Test_isWord(Str : String; Expected : Boolean) is
    begin
        Assert(isWord(Str) = Expected, "isWord(" & str & ") failed! " &
                "Expected " & Boolean'Image(Expected));
        Put_Line("    PASS: isWord(" & str & ")");
    end test_isWord;

    -- Tests the return value of randomInt() in 10,000 test trials
    procedure Test_randomInt(A : Integer; B : Integer) is
    Rand_Value : Integer;
    begin
        for i in 1 .. 10000 loop
            Rand_Value := randomInt(A, B);
            Assert(Rand_Value >= A and Rand_Value < B, "randomInt(" 
                    & Integer'Image(A) & "," & Integer'Image(B) & ") failed!"
                    & " Got " & Integer'Image(Rand_Value));
        end loop;
        Put_Line("    PASS: randomInt(" & Integer'Image(A) & "," & Integer'Image(B) & ")");
    end Test_randomInt;

    -- Testing variables

    -- Main variables

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
    New_Line;

    Put_Line("All tests passed!");

    -- Main code

end Wordscram;

-----------------------------------------------------------------------
