-module (mascaron).

-export ([run/1]).

run(Dir) ->
    Files = filelib:wildcard(Dir ++ "/*.xls"),
    {ok, FD} = file:open(Dir ++ ".csv", [write]),
    io:fwrite(FD, "~p~n", [Dir]),
    [First | _] = Results = [process_file(F) || F <- Files],
    {_, _, _, _, _, Replies} = First,
    ids_line(FD, Replies),
    header_line(FD, length(Replies)),
    dump_mascarons(FD, Results),
    Sums = sums(Results),
    N = length(Results),
    dump_sums(FD, Sums, N),
    Sorted = sort_questions(Sums),
    dump_sorted(FD, Sorted, N),
    ok = file:close(FD).

process_file(File) ->
    io:fwrite("mascaron:process_file/1:File=~p~n",[File]),
    CSV = File ++ ".csv",
    Res = os:cmd("ssconvert \"" ++ File ++ "\" \"" ++ CSV ++ "\""),
    io:fwrite("CSV conversion=~p~n",[Res]),
    {ok, FE} = file:open(CSV, [read]),
    {ok, Line1} = file:read_line(FE),
    [Outfile | _] = string:tokens(Line1, ","),
    {ok, Line2} = file:read_line(FE),
    [_, Name, _, Age | _] = string:tokens(Line2, ","),
    {ok, Line3} = file:read_line(FE),
    [_, Date, _, Gender | _] = string:tokens(Line3, ","),
    {ok, Line4} = file:read_line(FE),
    Mascarons = mascarons (string:tokens(Line4, ",\n")),
    Replies = aggregate (FE, Mascarons, file:read_line(FE)),
    {Outfile, Name, Age, Date, Gender, Replies}.

mascarons ([Name, _ | Tail]) ->
    [{Name, []} | mascarons(Tail)];
mascarons ([]) ->
    [].

aggregate (FE, Mascarons, {ok, Data}) ->
    [_ | Columns] = string:tokens(string:strip(Data, right, $\n), ","),
    Zip = zip_question (Mascarons, Columns),
    aggregate (FE, Zip, file:read_line(FE));
aggregate (_, Mascarons, eof) ->
    Mascarons.

zip_question([{Mascaron, List} | Mascarons], [Resp, Time | Columns]) ->
    [{Mascaron, List ++ [{Resp, Time}]} | zip_question (Mascarons, Columns)];
zip_question([], []) ->
    [].

ids_line(FD, Mascarons) ->
    io:fwrite(FD, "Mascarons;;;;", []),
    [io:fwrite(FD, "~s;;;;;;;;;;;;;;;;;;;;", [M]) || {M, _} <- Mascarons],
    io:fwrite(FD, "~n", []).

header_line(FD, N) ->
    io:fwrite(FD, "Fichier;Age;Sexe;Date;", []),
    [mascaron_headers(FD, X) || X <- lists:seq(1, N)],
    io:fwrite(FD, "~n", []).

mascaron_headers(FD, _) ->
    [io:fwrite(FD, "Q~p;Q~p temps;", [N,N]) || N <- lists:seq(1, 10)].

dump_mascarons(FD, [{_, Name, Age, Date, Gender, Replies} | Results]) ->
    io:fwrite(FD, "~p;~p;~p;~p;", [Name, Age, Gender, Date]),
    dump_replies(FD, Replies),
    io:fwrite(FD, "~n", []),
    dump_mascarons(FD, Results);
dump_mascarons(_, []) ->
    ok.

dump_replies(FD, [{_, List} | Replies]) ->
    [io:fwrite(FD, "~p;~p;", [Q, QT]) || {Q, QT} <- List],
    dump_replies(FD, Replies);
dump_replies(_, []) ->
    ok.

sums(Results) ->
    Lists = [List || {_,_, _,_,_,List} <- Results],
    lists:flatten(sum_lists (Lists)).

dump_sums(FD, Sums, N) ->
    io:fwrite(FD, ";;;;", []),
    [io:fwrite(FD, "~p;~p;", [S/N, T/N]) || {_, {S, T}} <- Sums],
    io:fwrite(FD, "~n", []).

sum_lists ([[] | _]) ->
    [];
sum_lists (Lists) ->
    Mascaron = [hd(List) || List <- Lists],
    Tail = [tl(List) || List <- Lists],
    [sum(Mascaron) | sum_lists(Tail)].

sum([{Name, Line} | _] = Mascaron) ->
    Stats = [S || {_, S} <- Mascaron],
    Init = lists:duplicate(length(Line), {0, 0}),
    Sums = lists:foldl(fun(Questions, Acc) ->
                               lists:zipwith(fun({QS,QT}, {AS, AT}) ->
                                                     {list_to_integer(QS)+AS,
                                                      list_to_integer(QT)+AT}
                                             end, Questions, Acc)
                       end, Init, Stats),
    {_, Tagged} = lists:foldl(fun(Sum, {Index, Acc}) ->
                                      {Index + 1, [{{Name, Index}, Sum} | Acc]}
                              end, {1, []}, Sums),
    lists:reverse(Tagged).

sort_questions(Sums) ->
    [sort_question(I, Sums) || I <- lists:seq(1,10)].

sort_question(I, Sums) ->
    Questions = lists:filter(fun({{_, N}, _}) ->
                                     N == I
                             end, Sums),
    lists:sort(fun({_, {Val1, _}}, {_, {Val2, _}}) ->
                       Val1 > Val2
               end, Questions).

dump_sorted(FD, Sorted, N) ->
    io:fwrite(FD, "Meilleurs scores:~n", []),
    [dump_sorted_question(FD, Question, N)|| Question <- Sorted].

dump_sorted_question(FD, [{{Name, Q}, {Val, _}} | Tail], N) ->
    io:fwrite(FD, "Q~p;~p;~p;", [Q, Name, Val/N]),
    dump_sorted_tail(FD, Tail, N).

dump_sorted_tail(FD, [], _) ->
    io:fwrite(FD, "~n", []);
dump_sorted_tail(FD, [{{Name, _}, {Val, _}} | Tail], N) ->
    io:fwrite(FD, "~p;~p;", [Name, Val/N]),
    dump_sorted_tail(FD, Tail, N).
