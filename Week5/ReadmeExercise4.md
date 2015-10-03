# Exercise 4


##Generator
The generator can be found in exercise4.hs. 


##Short report on findings
It is possible to create sudoku’s with three, four and five, six, seven, eight and nine empty blocks. But you will get multiple solutions when you have four or more blocks that are blank. With three blocks empty blocks diagonal you won’t get any problem. 

You can generate a sudoku with three empty blocks with:
> genThreeEmptyBlocks

With four empty blocks you need to execute:
> genFourEmptyBlocks

And a sudoku with five empty blocks you need to execute:
> genFiveEmptyBlocks

You will see that “genFourEmptyBlocks” and “genFiveEmptyBlocks” will not give four or five empty blocks.

##Time Spent
Time spent: 8 hours. Reason i’ve made an other solution. This can be found in Exercise4Debug.hs
