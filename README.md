# icfp-contest-2016


[Problem Description](http://icfpc2016.blogspot.com/2016/08/task-description.html)

Solution Example:
# Starting Point
![starting](/images/solution_src.png)
# Ending Point
![Ending](/images/solution_dest.png)

# Solution (With verticies mapping for easier reading)
```
7                    -- Number of Vertices
0,0                  -- 0

1,0                  -- 1

1,1                  -- 2

0,1                  -- 3

0,1/2                -- 4
1/2,1/2              -- 5
1/2,1                -- 6
4                    -- Number of facets
4 0 1 5 4            -- Facet
4 1 2 6 5            -- Facet
3 4 5 3              -- Facet
3 5 6 3              -- Facet
0,0                  -- Final Pos: 0
1,0                  -- Final Pos: 16
0,0                  -- Final Pos: 2
0,0                  -- Final Pos: 3
0,1/2                -- Final Pos: 4
1/2,1/2              -- Final Pos: 5
0,1/2                -- Final Pos: 6
```
