# draft-value

##  Draft Value Over Expectaion in the NFL

This code takes data gathered from [Pro Football Reference](https://www.pro-football-reference.com/), cleans it, and predicts the AV of every NFL pick back to 1989.

Use the function `get_team_table()` to get every draft class for a specific team, from the year provided up until the most recent season.

For example, `get_team_table("SEA", 2010)` produces the table below

![seahawks example](https://github.com/danmorse314/draft-value/blob/main/seahawks%20pcjs%20draft%20value.png)

Use the `get_team_plot()` function to see a line graph depicting the value over average over time for a specific team
