-module (mk_stats).

-export ([run/1]).

-define (Baseline_history, 25).
-define (Sample_size, 375).
-define (Min_fix_lines, 94).

-define (val(K,P), proplists:get_value(K, P)).
-define (val(K,P,D), proplists:get_value(K, P, D)).
-define (flat(L), lists:flatten(L)).

-record(state,
        {id, recording_name, recording_date, baseline, event_type, old_media,
         stats = []}).

-record(media_stats,
        {media_name, event_type, distance_left, distance_right,
         pupil_left, pupil_right}).

run (Files) ->
    Stats = [file(File) || File <- Files],
    headers(),
    [dump_stats(S) || S <- Stats].

file (File) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\r\n">>, [global]),
    Baseline = queue:new(),
    parse(tl(Lines), #state{baseline=Baseline}).

parse ([], State) -> State;
parse ([<<>> | Lines], State) -> parse(Lines, State);
parse ([Line | Lines],
       #state{baseline = Baseline, event_type = Previous_type,
              old_media = Old_media, stats = Stats} = State) ->
    [_, Id, Recording_name, Recording_date, _, _, _, Event_type, _,
     Distance_left, Distance_right, Pupil_left, Pupil_right, Media_name | _] =
        binary:split(Line, <<"\t">>, [global]),
    State2 = State#state{id = Id, recording_name = Recording_name,
                         recording_date = Recording_date},
    Data = #media_stats{media_name = Media_name,
                        event_type = Event_type,
                        distance_left = c_float(Distance_left),
                        distance_right = c_float(Distance_right),
                        pupil_left = fixation_float(Event_type, Pupil_left),
                        pupil_right = fixation_float(Event_type, Pupil_right)},
    State3 =
        case Media_name of
            %% nothing interesting here
            Old_media when Old_media == <<>> ->
                Baseline2 = queue:in(Data, Baseline),
                Baseline3 =
                    case queue:len(Baseline2) of
                        X when X > ?Baseline_history ->
                            element(2, queue:out(Baseline2));
                        _ -> Baseline2
                    end,
                State2#state{baseline = Baseline3, event_type = Event_type};
            %% we changed from Media to nothing
            <<>> ->
                Baseline2 = queue:in(Data, queue:new()),
                State2#state{baseline = Baseline2,
                             event_type = Event_type,
                             old_media = <<>>};
            %% Same Media, update stats
            Old_media ->
                {Nb_lines, Media_baseline, Existing_saccades,
                 Existing_fixations, Existing_stats} =
                    ?val(Media_name, Stats),
                if (Nb_lines =< ?Sample_size) ->
                        Saccades = saccades(Event_type, Existing_saccades),
                        Fixations =
                            fixations(Existing_fixations, Previous_type, Event_type),
                        New_stats =
                            {Media_name, {Nb_lines + 1, Media_baseline, Saccades, Fixations,
                                          Existing_stats ++ [Data]}},
                        Stats2 = lists:keyreplace(Media_name, 1, Stats, New_stats),
                        State2#state{baseline = undefined, event_type = Event_type,
                                     stats = Stats2};
                   true ->
                        State2#state{baseline = undefined, event_type = Event_type}
                end;
            %% New media
            _ ->
                Media_baseline = queue:to_list(Baseline),
                Saccades = saccades(Event_type, 0),
                Fixations = fixations({0, 0}, undefined, Event_type),
                Stats2 = [{Media_name,
                           {1, Media_baseline, Saccades, Fixations, [Data]}}
                          | Stats],
                State2#state{baseline = undefined, event_type = Event_type,
                            old_media = Media_name, stats = Stats2}
        end,
    parse (Lines, State3).

saccades (<<"Saccade">>, N) -> N+1;
saccades (_, N) -> N.

fixations ({Distinct, Total}, Previous, <<"Fixation">>)
  when Previous /= <<"Fixation">> -> {Distinct+1, Total+1};
fixations ({Distinct, Total}, _, <<"Fixation">>) -> {Distinct, Total+1};
fixations ({D,T}, _, _) -> {D,T}.

fixation_float (<<"Fixation">>, Value) -> c_float(Value);
fixation_float (_, _) -> undefined.

c_float (<<>>) -> undefined;
c_float (Bin) ->
    list_to_float(binary_to_list(binary:replace(Bin, <<",">>, <<".">>))).

sum (undefined, Acc) -> Acc;
sum (0.0, Acc) -> Acc;
sum (X, undefined) -> {X, 1, X, X};
sum (X, {Total, Count, Min, Max}) ->
    Min2 = if X < Min -> X;
              true -> Min
           end,
    Max2 = if X > Max -> X;
              true -> Max
           end,
    {Total + X, Count +1, Min2, Max2}.

headers_parts (Prefix, List) ->
    string:join([Prefix ++ header(H) || H <- List], "\t").

header (pa) -> "Mean Pupil";
header (pmax) -> "Max Pupil";
header (da) -> "Mean Distance";
header (dl) -> "Mean DistanceLeft";
header (dr) -> "Mean DistanceRight".

headers () ->
    Global_baseline =
        [[Emotion] ++ " Mean saccades\t" ++
             [Emotion] ++ " Mean fixations\t" ++
             [Emotion] ++ " TimeFixation\t" ++
             [Emotion] ++ " %Fixations\t" ++
             headers_parts([Emotion] ++ " Baseline ", [pa]) ++ "\t" ++
             headers_parts([Emotion] ++ " Media ", [da, dl, dr, pa, pmax])
         || Emotion <- emotions()],
    Medias =
        lists:flatmap(
          fun(N) ->
                  Prefix  = "Media " ++ integer_to_list(N) ++ " ",
                  [Prefix, "name\t", Prefix, "Nb saccades\t",
                   Prefix, "Nb fixations\t", Prefix, "TimeFixation\t",
                   Prefix, "%Fixations\t",
                   headers_parts(Prefix, [da, dl, dr, pa, pmax]), "\t",
                   headers_parts(Prefix ++ "Baseline ", [da, dl, dr, pa]),
                   "\t"]
          end, lists:seq(1, 36)),
    io:fwrite("Participant\tRecording_name\tRecording_date\tNb medias\t"
              "Rejected\t" ++
                  string:join(Global_baseline, "\t") ++ "\t" ++ Medias ++
                  "\n").

emotions () ->
    [$N, $J, $P, $C, $T, $D].

dump_stats (#state{id = Id, recording_name = Recording,
                   recording_date = Recording_date,
                   stats = Stats}) ->
    {Rejected, Stats2} = rejected(Stats),
    Emotion_stats = string:join([emotion_stats(E, Stats2)
                                 || E <- emotions()], "\t"),
    Emotion_stats_comma = commafy(Emotion_stats),
    Media_stats = all_media_stats(Stats),
    Rejected_list = string:join([binary_to_list(R) || R <- Rejected], " "),
    Nb_medias = length(Stats2),
    io:fwrite("~s\t~s\t~s\t~p\t~s\t~s\t~s~n",
              [Id, Recording, Recording_date, Nb_medias, Rejected_list,
               Emotion_stats_comma, Media_stats]).

rejected (Stats) ->
    lists:foldl(
      fun({_, {_, _, _, {_, Fix_lines}, _}} = Media, {Rej, Acc})
            when Fix_lines > ?Min_fix_lines -> {Rej, [Media | Acc]};
         ({Name, _}, {Rej, Acc}) -> {[Name | Rej], Acc}
      end, {[], []}, Stats).

commafy (Stats) ->
    re:replace(Stats, "\\.", ",", [{return, list}, global]).

init_stats () -> {undefined, undefined, undefined, undefined}.

emotion_stats(E, All_stats) ->
    Medias = emotion_medias(E, All_stats),
    {Lines_sum, Saccades_sum, {Fix_distinct, Fix_total},
     Base_stats, Global_stats} =
        lists:foldl(
          fun({_, {Nb_lines, Baseline, Saccades, Fixations, Stats}},
              {Nb_lines_acc, Saccades_acc, Fixations_acc, Base_acc, Stats_acc}) ->
                  {Nb_lines + Nb_lines_acc,
                   Saccades + Saccades_acc,
                   sum_fixations(Fixations, Fixations_acc),
                   stats(Baseline, Base_acc), stats(Stats,Stats_acc)}
          end, {0, 0, {0, 0}, init_stats(), init_stats()}, Medias),
    {Saccades_avg, Fix_distinct_avg} =
        case length(Medias) of
            0 -> {0, 0};
            Media_count -> {Saccades_sum / Media_count, Fix_distinct / Media_count}
        end,
    Time_fixation = Fix_total * 8,
    Percent = case Lines_sum of
                  0 -> 0;
                  _ -> (Time_fixation*100)/(Lines_sum*8)
              end,
    ?flat(io_lib:format("~p\t~p\t~p\t~p\t~s\t~s",
                        [Saccades_avg, Fix_distinct_avg,
                         Time_fixation, Percent,
                         format_stats(Base_stats, [pa]),
                         format_stats(Global_stats, [da, dla, dra, pa, pmax])])).

sum_fixations ({Distinct1, Total1}, {Distinct2, Total2}) ->
    {Distinct1 + Distinct2, Total1 + Total2}.

stats (Stats, Acc) ->
    lists:foldl(
      fun(#media_stats{distance_left  = DL,
                       distance_right = DR,
                       pupil_left = PL,
                       pupil_right = PR},
          {DLA, DRA, PLA, PRA}) ->
              {sum(DL, DLA), sum(DR, DRA),
               sum(PL, PLA), sum(PR, PRA)}
      end, Acc, Stats).

emotion_medias (E, Stats) ->
    lists:filter(
      fun({Media_name, _}) ->
              case binary:part(Media_name, {size(Media_name), -5}) of
                  <<E, _/binary>> -> true;
                  _ -> false
              end
      end, Stats).

all_media_stats (Stats) ->
    string:join([media_stats(S) || S <- lists:usort(Stats)], "\t").

media_stats ({Media_name,
              {Nb_lines, Baseline, Saccades,
               {Distinct_fixations, Total_fixations}, Stats}}) ->
    Time_fixation = Total_fixations*8,
    Base_stats = format_stats(stats(Baseline, init_stats()), [da, dla, dra, pa]),
    Media_stats = format_stats(stats(Stats, init_stats()), [da, dla, dra, pa, pmax]),
    binary_to_list(Media_name) ++ "\t" ++
        commafy(?flat(io_lib:format("~p\t~p\t~p\t~p\t~s\t~s",
                                    [Saccades, Distinct_fixations,
                                     Time_fixation, (Time_fixation*100)/(Nb_lines*8),
                                     Media_stats, Base_stats]))).

format_stats (Values, Fields) ->
    string:join([format_stat(Values, Field) || Field <- Fields], "\t").

format_stat ({DL, _, _, _}, dla) -> format_average(DL);
format_stat ({_, DR, _, _}, dra) -> format_average(DR);
format_stat ({DL, DR, _, _}, da) -> format_average(DL, DR);
format_stat ({_, _, PL, _}, pla) -> format_average(PL);
format_stat ({_, _, _, PR}, pra) -> format_average(PR);
format_stat ({_, _, PL, PR}, pa) -> format_average(PL, PR);
format_stat ({_, _, PL, PR}, pmax) -> format_max(PL, PR).

format_average (L, R) ->
    Mean =
        case {L, R} of
            {undefined, undefined} -> "";
            {X, undefined} -> format_average(X);
            {undefined, X} -> format_average(X);
            {{T1, C1, _, _}, {T2, C2, _, _}} -> io_lib:format("~p", [(T1+T2)/(C1+C2)])
        end,
     ?flat(Mean).

format_average (undefined) -> "";
format_average ({Total, Count, _Min, _Max}) ->
    ?flat(io_lib:format("~p", [Total/Count])).

format_max (undefined, undefined) -> "";
format_max (undefined, {_, _, _, Max}) -> ?flat(io_lib:format("~p", [Max]));
format_max ({_, _, _, Max}, undefined) -> ?flat(io_lib:format("~p", [Max]));
format_max ({_, _, _, Max1}, {_, _, _, Max2}) ->
    ?flat(io_lib:format("~p", [(Max1+Max2)/2])).
