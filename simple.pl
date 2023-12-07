% Movie facts
movie("Toy Story", 1995, ["Adventure", "Animation", "Comedy"], 3.9, ["family", "fun"]).
movie("Jumanji", 1995, ["Adventure", "Fantasy"], 3.5, ["action", "thriller"]).
movie("Heat", 1995, ["Action", "Crime", "Drama"], 4.0, ["heist", "suspense"]).

% Predicate to check if a genre is in a list of genres.
genre_in_list(Genre, Genres) :-
    member(Genre, Genres).

% Predicate to filter movies based on genre.
movie_filter(Genre, Title) :-
    movie(Title, _, Genres, _, _),
    genre_in_list(Genre, Genres).

% Write out movie titles.
write_movies([]).
write_movies([Head|Tail]) :-
    writeln(Head),
    write_movies(Tail).

% Ask the user for their preferred genre and filter movies based on that.
ask_genre_and_filter() :-
    writeln("What genre are you interested in?"),
    read(UserInput),
    atom_string(Genre, UserInput),  % Convert user input to string if necessary
    findall(Title, movie_filter(Genre, Title), Movies),
    format("Movies in the genre ~w: ~n", [Genre]),
    write_movies(Movies).

% Start the process.
start :-
    ask_genre_and_filter().
