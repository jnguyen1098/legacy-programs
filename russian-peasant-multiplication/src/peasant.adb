-----------------------------------------------------------------------------
--                      Russian Peasant Multiplication                     --
--                        By: Jason Nguyen (XXXXXXXX)                       --
--                                 XXXXXXXX                                --
-----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure Peasant is

------------------------------ Main Subprograms -------------------------------

    function russianPeasantRecursive(m, n : Long_Integer) return Long_Integer;
    function russianPeasantIterative(ml, mn : Long_Integer) return Long_Integer;

    procedure benchmarkRecursive(m, n : Long_Integer);
    procedure benchmarkIterative(m, n : Long_Integer);

-------------------------------------------------------------------------------

    -- Recursive Russian Peasant Multiplication
    function russianPeasantRecursive(m, n : Long_Integer) return Long_Integer is
    begin
        -- Base cases
        if m = 0 then
            return 0;
        elsif m = 1 then
            return n;
        -- Recursive cases
        elsif m mod 2 = 0 then
            return russianPeasantRecursive(m / 2, n * 2);
        else
            return n + russianPeasantRecursive(m / 2, n * 2);
        end if;
    end russianPeasantRecursive;

    -- Iterative Russian Peasant Multiplication
    function russianPeasantIterative(ml, mn : Long_Integer) return Long_Integer is
        m       :   Long_Integer := ml;
        n       :   Long_Integer := mn;
        result  :   Long_Integer := 0;
    begin
        -- Continually reduce m until it is 0
        -- while raising n by a factor of 2
        while m > 0 loop
            if m mod 2 = 1 then
                result := result + n;
            end if;
            m := m / 2;
            n := n * 2;
        end loop;
        return result;
    end russianPeasantIterative;

    -- Benchmark recursive function -- multiplies all
    -- numbers within a certain range, sequentially
    procedure benchmarkRecursive(m, n : Long_Integer) is
        start  : Time;
        finish : Time;
        result : Long_Integer;
    begin
        -- Clock start
        start := Clock;
        for i in m .. n loop
            for j in m .. n loop
                result := russianPeasantRecursive(i, j);
            end loop;
        end loop;
        if result < 0 then Put_Line("Error"); end if;
        -- Clock end
        finish := Clock;
        Put_Line("  Recursive took" & Duration'Image(finish - start) & " seconds");
    end benchmarkRecursive;

    -- Benchmark recursive function -- multiplies all
    -- numbers within a certain range, sequentially
    procedure benchmarkIterative(m, n : Long_Integer) is
        start  : Time;
        finish : Time;
        result : Long_Integer;
    begin
        -- Clock start
        start := Clock;
        for i in m .. n loop
            for j in m .. n loop
                result := russianPeasantIterative(i, j);
            end loop;
        end loop;
        if result < 0 then Put_Line("Error"); end if;
        -- Clock end
        finish := Clock;
        Put_Line("  Iterative took" & Duration'Image(finish - start) & " seconds");
    end benchmarkIterative;

------------------------------ Variables ------------------------------

    multiplier      :   Long_Integer;
    multiplicand    :   Long_Integer;
    recursiveAns    :   Long_Integer;
    iterativeAns    :   Long_Integer;

    -- My test suite (it's an array of ranges to use)
    rng : constant Array(0 .. 4) of Long_Integer := (1,5,50,500,5000);

-------------------------------- Main ---------------------------------

begin

    Put_Line("-------------------------------------------");
    Put_Line("-- Russian Peasant Multiplication in Ada --");
    Put_Line("--    Enter a negative number to quit    --");
    Put_Line("--       By Jason Nguyen (XXXXXXXX)       --");
    Put_Line("-------------------------------------------");

    -- Benchmarks...
    delay Duration(1.0);
    Put_Line("Please wait...running startup benchmarks...");
    New_Line;
  
    -- Run every test
    for i in 1 .. 4 loop
        delay Duration(1.0);
        -- Printline
        Put_Line("Multiplying every number from" &
                Long_Integer'Image(rng(i - 1)) &
                " to" & Long_Integer'Image(rng(i)));
        -- Recursive benchmark
        benchmarkRecursive(rng(i - 1), rng(i));
        -- Iterative benchmark
        benchmarkIterative(rng(i - 1), rng(i));
        delay Duration(1.0);
        New_Line;
    end loop;

    loop
        -- User input
        Put_Line("Enter a positive number (or negative to quit):");
        Put("> ");
        Get(multiplier);

        -- Early termination on negative
        if multiplier < 0 then exit; end if;

        -- User input again
        New_Line;
        Put_Line("Enter another one (or negative to quit):");
        Put("> ");
        Get(multiplicand);

        -- Early termination on negative
        if multiplicand < 0 then exit; end if;

        -- Calculate answers
        recursiveAns := russianPeasantRecursive(multiplier, multiplicand);
        iterativeAns := russianPeasantIterative(multiplier, multiplicand);
        New_Line;

        -- Print recursive answer
        Put_Line("(recursive)" & Long_Integer'Image(multiplier) & "*" &
                Long_Integer'Image(multiplicand) & "=" & Long_Integer'Image(recursiveAns));

        -- Print iterative answer
        Put_Line("(iterative)" & Long_Integer'Image(multiplier) & "*" &
                Long_Integer'Image(multiplicand) & "=" & Long_Integer'Image(iterativeAns));
        New_Line;
    end loop;

    New_Line;
    Put_Line("Thanks for using this program!");

end Peasant;

-----------------------------------------------------------------------
