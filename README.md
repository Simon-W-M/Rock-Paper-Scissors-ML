# Rock Paper Scissors ML

Wanted to play with shiny and also build a ML model for Rock Paper Scissors

## Strategies

Started with these I found on the web

. The Opening Move: Statistically, rock is thrown most often (35.4%), followed by scissors (34.6%) and paper (30%). Therefore, opening with paper is a solid, aggressive, and often winning strategy.
. Win-Stay, Lose-Shift: Research shows players who lose tend to change their move, while winners often stick with their winning move.
. If you lose: Assume your opponent will throw the same move again. Play the move that beats it.
. If you win: Switch to the move that beats the one you just threw (e.g., if you won with paper, switch to scissors).
. The "Same Move" Counter: If your opponent throws the same move twice in a row, they are likely to change it on the third try. Use this to anticipate their next move (e.g., if they play rock twice, they may play paper next, so throw scissors).

## ML

Then tried to add some ML.

Got lots of help from gemini to build the code here but had to stitch it all together.

## Evaluation

I've been playing with it and as I designed it's core strategies I kind of know how to beat it, however it does do quite well.  
I would love some fresh players to have a go and let me know if it does have a win rate better than 33% which would be pure chance.
