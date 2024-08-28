-module(islands).

-compile(export_all).

-spec count_sub_islands(Grid1 :: [[integer()]], Grid2 :: [[integer()]]) -> integer().
count_sub_islands(Grid1, Grid2) ->
    % Convert lists to {x,y} => value maps
    Grid1_Map = convert_grid_to_map(Grid1),
    Grid2_Map = convert_grid_to_map(Grid2),
    % Create a map to track the visited squares on the second grid
    VisitedMap = maps:map(fun(_, _) -> not_visited end, Grid2_Map),
    % Find the islands on the second grid
    Islands = find_islands(Grid2_Map, VisitedMap),
    % For each island found, check if EVERY square is a 1 on the first grid
    find_amount_of_islands_on_grid_1(Islands, Grid1_Map).

convert_grid_to_map(Grid) ->
    % Double loop over the 2d list and build a map of the coordinates
    {_, Map} =
        lists:foldl(fun(Row, {X, AccIn}) ->
            {_, Temp} =
                lists:foldl(fun(Value, {Y, AccIn2}) ->
                    {Y + 1, maps:put({X,Y}, Value, AccIn2)}
                end, {0, AccIn}, Row),
            {X + 1, Temp}
        end, {0, #{}}, Grid),
    Map.

find_islands(GridMap, VisitedMap) ->
    % For each square, if it has not been visited before, perform a depth/breadth first search
    % to find all coordinates connected that have a value of 1
    % look right, then down, then left, then up
    {_, Islands} = find_islands_recursively(GridMap, VisitedMap),
    % io:format("New Visited Map FINAL: ~p~n", [NewVisitedMap]),
    % io:format("Found Islands List FINAL: ~p~n", [Islands]),
    Islands.

find_islands_recursively(GridMap, VisitedMap) ->
    % loop over the map, performing the search starting from each square on the grid
    % Accumulator is the {VisitedMap, AccumulatedIslands}
    maps:fold(fun(Coordinate, Value, {CurrentVisitedMap, AccumulatedIslands}) ->
        % do stuff
        % for each square in the map, start a depth first search which will return a new visited map and a list of squares that make an island
        % or if the square is visited already or a 0, it will return an empty list
        case find_islands_recursively_(GridMap, CurrentVisitedMap, Coordinate, Value, maps:get(Coordinate, CurrentVisitedMap, visited), []) of
            {NewVisitedMap, []}              -> {NewVisitedMap, AccumulatedIslands};
            {NewVisitedMap, FoundIslandList} -> {NewVisitedMap, [FoundIslandList | AccumulatedIslands]}
        end
    end, {VisitedMap, []}, GridMap).

% If current square visited, or a 0, abort and return AccumulatedIsland
find_islands_recursively_(_, Map, Coordinate, 0, _, AccumulatedIsland) ->
    {maps:put(Coordinate, visited, Map), AccumulatedIsland};
find_islands_recursively_(_, Map, _, _, visited, AccumulatedIsland) ->
    {Map, AccumulatedIsland};
find_islands_recursively_(GridMap, VisitedMap, CurrentCoordinate = {X, Y}, 1, not_visited, AccumulatedIsland) ->
    % This square is a 1 and has not been visted
    % add myself to the accumulated island and visited map
    VisitedMap1 = maps:put(CurrentCoordinate, visited, VisitedMap),
    AccumulatedIsland1 = [CurrentCoordinate | AccumulatedIsland],
    % depth search in each direction, update the accumulated island and visited map before choosing another direction
    % RIGHT
    RightCoordinate = {X + 1, Y},
    {VisitedMap2, AccumulatedIsland2} = find_islands_recursively_(GridMap, VisitedMap1, RightCoordinate, maps:get(RightCoordinate, GridMap, 0), maps:get(RightCoordinate, VisitedMap1, visited), AccumulatedIsland1),
    % DOWN
    DownCoordinate = {X, Y - 1},
    {VisitedMap3, AccumulatedIsland3} = find_islands_recursively_(GridMap, VisitedMap2, DownCoordinate, maps:get(DownCoordinate, GridMap, 0), maps:get(DownCoordinate, VisitedMap2, visited), AccumulatedIsland2),
    % LEFT
    LeftCoordinate = {X - 1, Y},
    {VisitedMap4, AccumulatedIsland4} = find_islands_recursively_(GridMap, VisitedMap3, LeftCoordinate, maps:get(LeftCoordinate, GridMap, 0), maps:get(LeftCoordinate, VisitedMap3, visited), AccumulatedIsland3),
    % UP
    UpCoordinate = {X, Y + 1},
    {VisitedMap5, AccumulatedIsland5} = find_islands_recursively_(GridMap, VisitedMap4, UpCoordinate, maps:get(UpCoordinate, GridMap, 0), maps:get(UpCoordinate, VisitedMap4, visited), AccumulatedIsland4),
    % once that has been done, return the final updated visited map and accumulated island list
    % io:format("COMPLETED A SEARCH!!~nVISITED MAP: ~p~nACCUMULATED ISLAND: ~p~n", [VisitedMap5, AccumulatedIsland5]),
    {VisitedMap5, AccumulatedIsland5}.

find_amount_of_islands_on_grid_1(Islands, Grid1_Map) ->
    % for each list of coordinates in the islands list, check if each one is in the grid1 map
    lists:foldl(fun(Island, AmountOfIslandsMatched) ->
        % if island is on grid1, increment amount of islands matched
        case is_island_on_grid_1(Island, Grid1_Map) of
            true -> AmountOfIslandsMatched + 1;
            _    -> AmountOfIslandsMatched
        end
    end, 0, Islands).

is_island_on_grid_1([], _) ->
    true;
is_island_on_grid_1([Coordinate | IslandList], Grid) ->
    case maps:get(Coordinate, Grid, 0) of
        1 -> is_island_on_grid_1(IslandList, Grid);
        _ -> false
    end.
