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

# Valid solution and Normalized solution

A solution is valid if and only if it satisfies all of the following conditions:

0.    All the source positions of the vertices are within the initial square spanned by the four vertices (0,0), (1,0), (1,1), (0,1).
1.    No coordinate appears more than once in the source positions part.
2.    Any edge of any facet has length greater than zero.
3.    At source positions, if two different edges share a point, the point should always be one of the endpoints for both the edges. That is, an edge touching another edge, or edges crossing each other are prohibited.
4.    All facet polygons are simple; a facet polygon’s perimeter must not intersect itself.
5.    Every facet at source position maps to its destination position, by a congruent transformation that maps its source vertices to corresponding destination vertices.
6.    At source position, the intersection set of any two different facets has zero area.
7.    At source position, the union set of all facets exactly matches the initial square.
8.    The size of the solution is no larger than Bs = 5000 Bytes.

Note that all facets are defined to have positive areas, regardless of their perimeters being clockwise/counterclockwise, as described in “The facets part” section.

# A solution is normalized if and only if it satisfies all of the following conditions:

0.   It is valid.
1.    At source position, if two different facets share an edge for a length greater than 0, then the intersection set of those two facets at destination positions must have an area greater than 0. In other words, if an edge separates two facets, you should always fold the origami at that edge.
