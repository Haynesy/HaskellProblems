-- (**) Flatten a nested list structure.

--  data NestedList a = Elem a | List [NestedList a]
-- *Main> flatten (Elem 5)
-- [5]
-- *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- *Main> flatten (List [])
-- []