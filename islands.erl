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
    {_, Islands} = find_islands(Grid2_Map, VisitedMap),
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
    maps:fold(fun(Coordinate, _, {CurrentVisitedMap, AccumulatedIslands}) ->
        % if the square is visited already or a 0, it will return an empty list
        case find_islands_(GridMap, CurrentVisitedMap, Coordinate, []) of
            {NewVisitedMap, []}              -> {NewVisitedMap, AccumulatedIslands};
            {NewVisitedMap, FoundIslandList} -> {NewVisitedMap, [FoundIslandList | AccumulatedIslands]}
        end
    end, {VisitedMap, []}, GridMap).

find_islands_(GridMap, VisitedMap, CurrentCoordinate = {X, Y}, AccumulatedIsland) ->
    case {maps:get(CurrentCoordinate, GridMap, 0), maps:get(CurrentCoordinate, VisitedMap, visited)} of
        {0, _} ->
            {maps:put(CurrentCoordinate, visited, VisitedMap), AccumulatedIsland}; % square is 0
        {_, visited} ->
            {VisitedMap, AccumulatedIsland}; % been visited before
        _ ->
            VisitedMap1 = maps:put(CurrentCoordinate, visited, VisitedMap),
            AccumulatedIsland1 = [CurrentCoordinate | AccumulatedIsland],
            % depth search in each direction, update the accumulated island and visited map before choosing another direction
            % RIGHT
            {VisitedMap2, AccumulatedIsland2} = find_islands_(GridMap, VisitedMap1, {X + 1, Y}, AccumulatedIsland1),
            % DOWN
            {VisitedMap3, AccumulatedIsland3} = find_islands_(GridMap, VisitedMap2, {X, Y - 1}, AccumulatedIsland2),
            % LEFT
            {VisitedMap4, AccumulatedIsland4} = find_islands_(GridMap, VisitedMap3, {X - 1, Y}, AccumulatedIsland3),
            % UP
            find_islands_(GridMap, VisitedMap4, {X, Y + 1}, AccumulatedIsland4)
    end.

find_amount_of_islands_on_grid_1(Islands, Grid1_Map) ->
    lists:foldl(fun(Island, AmountOfIslandsMatched) ->
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
