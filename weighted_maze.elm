-- import Maze
-- import Grid
-- import GridCell
-- import DistanceGrid
-- import WeightedGrid
-- import Random.Pcg as Random

-- -- create maze
-- m = Maze.init Maze.RecursiveBacktracker 10 10 (Random.initialSeed 123) Maze.Rect Maze.Ascii |> Maze.update

-- -- braid it
-- braidGrid = Grid.braid m.grid Grid.neighbors 0.5
-- start = GridCell.maybeGridCellToGridCell <| Grid.getCell braidGrid 0 0
-- finish = GridCell.maybeGridCellToGridCell <| Grid.getCell braidGrid 9 9

-- -- grid.distances = start.distances.path_to(finish)
-- wg = WeightedGrid.init braidGrid start
-- -- wg' = {wg | dists = DistanceGrid.pathTo wg start finish}
