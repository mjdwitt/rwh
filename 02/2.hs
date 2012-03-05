-- file: 02/2.hs
-- date: 10 Dec 2011
-- auth: Michael DeWitt
-- Exercise 2, Chapter 2:
--   Write a function `lastButOne`, that returns the element *before* the last.

-- This currently throws errors for lists that are shorter than two elements.
lastButOne list =
  if null (tail (tail list)) then
    head list
  else
    lastButOne (tail list)

-- A safer version of the above based on material from Chapter 3
safeLastButOne (x:(_:[])) = Just x
safeLastButOne []	  = Nothing
safeLastButOne (_:[])	  = Nothing
safeLastButOne (_:xs)	  = safeLastButOne xs

-- An abstraction of the above which returns the nth element of a list.

nthElement n list = if n <= 1 || null (tail list)
		    then head list
		    else nthElement (n - 1) (tail list)
