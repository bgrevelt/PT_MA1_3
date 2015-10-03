# Exercise 4


##Generator
The generator can be found in Exercise4.hs. 


##Short report on findings
It is possible to create sudoku’s with three, four and five, six, seven, eight and nine empty blocks. But you will get most of the time multiple solutions when you have four blocks, but is possible to generate some suduko's with four empty blocks. I was not able to generate a sudoku with five or more blocks that were blank. I assume that this is not possible. Getting a soloution with three blocks is pretty easy. Just empty three block diagonal and you will always get an unique solution. 

You can generate a sudoku with three empty blocks with:
> solutionThreeEmptyBlocks

To prove that it is possible to have a sudoku with four empty blocks you need to execute:
> solutionFourEmptyBlocks

For trying to generate a sudoku with four empty blocks you can execute the function:
> generatingFourEmptyBlocks

For trying to generate a sudoku with five empty blocks you can execute the function:
> genFiveEmptyBlocks

You will see that “generatingFourEmptyBlocks” maybe gives proof and that “genFiveEmptyBlocks” will not give five empty blocks.

##Time Spent
Time spent: 10 hours. Reason I’ve made an other solution. This can be found in Exercise4Debug.hs
