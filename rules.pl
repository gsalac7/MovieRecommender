% Predicate to split a string by a delimiter into a list of substrings.
split_string_by_delimiter(String, Delimiter, List) :-
    split_string(String, Delimiter, "", List).

% Predicate to check if an item is in a list.
item_in_list(Item, List) :-
    member(Item, List).

% Predicate to filter movies based on criteria.
movie_filter(Genre, Year, Rating, Tag, Title) :-
    movie(Title, MovieYear, Genres, MovieRating, Tags),
    (Genre = 'any' ; item_in_list(Genre, Genres)),
    (Year = 'any' ; MovieYear = Year),
    (Rating = 'any' ; MovieRating >= Rating),
    (Tag = 'none' ; item_in_list(Tag, Tags)).

% Predicate to ask user preferences and recommend movies.
ask_preferences() :-
    writeln("What genre are you interested in? (Type 'any' for no preference)"),
    read(Genre),
    writeln("What year are you interested in? (Type 'any' for no preference)"),
    read(Year),
    writeln("What minimum rating are you looking for? (Type 'any' for no preference)"),
    read(Rating),
    writeln("Any specific tags you're interested in? (Type 'none' for no preference)"),
    read(Tag),
    recommend_movies(Genre, Year, Rating, Tag).

recommend_movies(Genre, Year, Rating, Tag) :-
    findall(Title, movie_filter(Genre, Year, Rating, Tag, Title), Movies),
    list_to_set(Movies, UniqueMovies),
    (   UniqueMovies = []
    ->  writeln("No movies found with these criteria.")
    ;   writeln("Movies recommended for you: "),
        write_movies(UniqueMovies)
    ).

% Predicate to write movie titles.
write_movies([]).
write_movies([Head|Tail]) :-
    writeln(Head),
    write_movies(Tail).

% Start the recommendation process.
start :-
    ask_preferences().

