Is there a difference between the symmetric closure of the transitive closure 
of a relation R and the transitive closure of the symmetric closure of R

This was found using quickcheck:
*TestRel> quickCheck prop_Q8

> trClos [(0,1)]
      = [(0,1)]
> symClos [(0,1)]
      = [(0,1),(1,0)]

> symClos [(0,1)]
      = [(0,1),(1,0)]
> trClos  [(0,1),(1,0)]
      = [(0,0),(0,1),(1,0),(1,1)]
      
So they are not equal in this specific case which indicate that there is a difference.
