-module (rmie).

-export ([out/1]).

-include_lib ("yaws/include/yaws_api.hrl").

out (Arg) ->
    Params = ["reply_id", "reply"],
    [Reply_id, Reply] = [yaws_api: queryvar (Arg, Param)
			 || Param <- Params],
    Step = case {Reply_id, Reply} of
	       {undefined, undefined} -> 1;
	       {{ok, Id}, undefined} ->
		   list_to_integer (Id);
	       {{ok, Id}, {ok, Reply_value}} ->
		   ok = log_results (Id, Reply_value),
		   list_to_integer (Id) + 1
	   end,
    ehtml (Step).
    
step (Step) ->
    Steps = load_steps (),
    Details = if Step > length (Steps) ->
		      undefined;
		 true ->
		      lists: nth (Step, Steps)
	      end,
    {Details, length (Steps)}.

load_steps () ->
    CSV_path = filename: join (["RMIE.csv"]),
    {ok, Bin} = file: read_file (CSV_path),
    Text = binary_to_list (Bin),
    [_ | Lines] = string: tokens (Text, "\n"),
    [string: tokens (Line, ",") || Line <- Lines, length (Line) > 20].


ehtml (Step) ->
    case step (Step) of
	{undefined, _} ->
	    ehtml_end ();
	{Details, Total} ->
	    ehtml_picture (Step, Details, Total)
    end.

ehtml_end () ->
    {ehtml, [{html, [], [{body, [], "Fin"}]}]}.

ehtml_picture (Step, [Image, Choice1, Choice2, Choice3, Choice4, _], Total) ->
    {ehtml,
     [{html, [],
       [{head, [],
	 [{meta, [{'http-equiv', "Content-Type"},
		  {content, "text/html; charset=ISO-8859-1"}]},
	  {style, [{type, "text/css"}],
	   "html {font-family:Arial,sans-serif;\n"
	   "      font-size: 1.5em;}"}
	 ]},
	{body, [],
	 [{'div', [{style, "height: 100px"}], header (Step, Total)},
	  {'div', [{style, "float: left; width: 600px; padding-left: 50px;"}],
	   [{img, [{src, strip (Image)}, {alt, Image}]}]
	  },
	  {'div', [{style, "float: left; padding-top: 50px;"}],
	   [{form, [{action, "rmie"}, {method, "GET"}, {name, "form"}],
	     [
	      {input, [{type, "hidden"}, {name, "reply_id"},
		       {value, integer_to_list (Step)}]},
	      choices ([Choice1, Choice2, Choice3, Choice4]),
	      {input, [{type, "submit"}, {value, "Suivant"}]}]}]}]}]}]}.

header (1, Total) ->
    [count (1, Total),
     {'div',
      [{style, "padding-left: 50px; font-weight: bold; margin-top: 50px;"}],
      "Tâche d'interprétation du regard (adapté de S. Baron-Cohen)"},
     {'div',
      [{style, "padding-left: 50px; margin-top: 20px"}], 
      "Quel mot décrit le mieux ce que la personne sur la photo pense ou"
      " ressent?"}];
header (N, Total) ->
    [count (N, Total)].

count (N, Total) ->
    {'div', [{style, "float: right;"}],
     ["Photo ", integer_to_list (N), "/", integer_to_list (Total)]}.

choices (Choices) ->
    {_, EHTML} =
	lists: foldr (fun (Choice, {Index, Acc}) ->
			      {Index - 1, [choice (Index, Choice) | Acc]}
		      end, {length (Choices), []}, Choices),
    EHTML.

choice (Int, Choice) ->
    [{'div', [],
      [{input, [{type, "radio"}, {value, integer_to_list (Int)},
		{name, "reply"}]}, strip (Choice)]}].

log_results (Id, Reply) ->
    {Step, _} = step (list_to_integer (Id)),
    Correct_answer = correct_answer (Step),
    {ok, FD} = file: open ("RMIE_results.csv", [append]),
    Success = case Reply of
		  Correct_answer -> 1;
		  _ -> 0
	      end,
    io: fwrite (FD, "~p;~p;~s;~s;~s;~p~n",
		[date(), time(), Id, Reply, Correct_answer, Success]),
    file: close (FD).

correct_answer (Step) ->
    lists: nth (6, Step).

strip (String) ->
    string: strip (String, both, $").
