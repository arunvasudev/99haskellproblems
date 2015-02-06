-- Problem 22
-- Create a list containing numbers in a given range
range a b = if (a < b) then [a..b] else [a, (a - 1)..b]
