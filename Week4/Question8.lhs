Is there a difference between the symmetric closure of the transitive closure 
of a relation R and the transitive closure of the symmetric closure of R

let T =
      (1, 2)
      (2, 3)
      (3, 4)

> trClos [(1,2), (2,3), (3,4)] 
      = [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)]
> symClos [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)] 
      = [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4),(2,1),(3,2),(4,3),(3,1),(4,2),(4,1)]

> symClos [(1,2), (2,3), (3,4)]
      = [(1,2),(2,3),(3,4),(2,1),(3,2),(4,3)]
> trClos  [(1,2),(2,3),(3,4),(2,1),(3,2),(4,3)]
      = [(1,2),(2,3),(3,4),(2,1),(3,2),(4,3),(1,3),(1,1),(2,4),(2,2),(3,3),(3,1),(4,4),(4,2),(1,4),(4,1)]
      
So they are not equal in this specific case which indicate that there is a difference.
