% Import required library for random selection
:- use_module(library(random)).

% Define similar genres
similar_genre("Comedy", "Romance").
similar_genre("Romance", "Comedy").
similar_genre("Drama", "Romance").
similar_genre("Crime", "Thriller").
similar_genre("Thriller", "Mystery").
similar_genre("Horror", "Thriller").
similar_genre("Mystery", "Crime").
similar_genre("Action", "Adventure").
similar_genre("War", "Drama").
similar_genre("Adventure", "Fantasy").
similar_genre("Documentary", "Biography").
similar_genre("Sci Fi", "Fantasy").
similar_genre("Children", "Family").
similar_genre("Western", "Adventure").
similar_genre("Fantasy", "Sci Fi").
similar_genre("IMAX", "Documentary").


% Predicate to check if a genre is in a list of genres.
genre_in_list(Genre, Genres) :-
    member(Genre, Genres).

% Predicate to check if a tag is in a list of tags.
tag_in_list(Tag, Tags) :-
    member(Tag, Tags).

% Predicate to filter movies based on criteria.
movie_filter(Genre, Year, Rating, Tag, Title) :-
    movie(Title, MovieYear, Genres, MovieRating, Tags),
    (Genre = 'any' ; genre_in_list(Genre, Genres)),
    (Year = 'any' ; MovieYear = Year),
    (Rating = 'any' ; (number(Rating), MovieRating >= Rating)),
    (Tag = 'none' ; tag_in_list(Tag, Tags)).

% Find movies based on user preferences.
find_movies(Genre, Year, Rating, Tag, Movies) :-
    findall(Title-Year, movie_filter(Genre, Year, Rating, Tag, Title), Movies).

% take(+N, +List, -FirstN)
% Take the first N elements from a list.
take(N, List, FirstN) :-
    length(FirstN, N),
    append(FirstN, _, List).

% handle_recommendations(+Movies, +Genre, +Year, +Rating, +Tag)
handle_recommendations([], Genre, Year, Rating, Tag) :-
    Tag \= 'none',  % Check if a tag was specified
    writeln("No movies found with the specified tag. Trying without the tag..."),
    recommend_movies(Genre, Year, Rating, 'none').  % Retry without the tag
handle_recommendations([], Genre, Year, Rating, 'none') :-
    try_lower_rating(Genre, Year, Rating, 'none').  % If no tag or no movies found, try lowering rating
handle_recommendations(Movies, _, _, _, _) :-
    length(Movies, Count),
    (   Count >= 5
    ->  random_permutation(Movies, ShuffledMovies),
        take(5, ShuffledMovies, SelectedMovies)
    ;   SelectedMovies = Movies
    ),
    writeln("Movies recommended for you: "),
    write_movies(SelectedMovies).

% recommend_movies(+Genre, +Year, +Rating, +Tag)
recommend_movies(Genre, Year, Rating, Tag) :-
    find_movies(Genre, Year, Rating, Tag, Movies),
    handle_recommendations(Movies, Genre, Year, Rating, Tag).


% Try finding movies with a lower rating.
try_lower_rating(Genre, Year, Rating, Tag) :-
    (   Rating \= 'any', NewRating is Rating - 1, NewRating >= 1
    ->  writeln("Adjusting rating criteria to find matches..."),
        find_movies(Genre, Year, NewRating, Tag, NewMovies),
        handle_recommendations(NewMovies, Genre, Year, NewRating, Tag)
    ;   try_similar_genre(Genre, Year, Tag)
    ).

% Try finding movies with a similar genre.
try_similar_genre(Genre, Year, Tag) :-
    (   similar_genre(Genre, SimilarGenre)
    ->  writeln("Trying a similar genre..."),
        find_movies(SimilarGenre, Year, 'any', Tag, NewMovies),
        handle_recommendations(NewMovies, SimilarGenre, Year, 'any', Tag)
    ;   writeln("No similar genres or movies found.")
    ).

% Write out movie titles along with their year.
write_movies([]).
write_movies([Title-Year|Tail]) :-
    format('~w (~w)\n', [Title, Year]),
    write_movies(Tail).

% Ask user preferences and filter movies based on that.
ask_preferences_and_filter() :-
    writeln("What genre are you interested in? (Type 'any' for no preference)"),
    read(Genre),
    writeln("What year are you interested in? (Type 'any' for no preference)"),
    read(Year),
    writeln("What minimum rating are you looking for? (Type 'any' for no preference)"),
    read(Rating),
    writeln("Any specific tags you're interested in? (Type 'none' for no preference)"),
    read(Tag),
    find_movies(Genre, Year, Rating, Tag, Movies),
    handle_recommendations(Movies, Genre, Year, Rating, Tag).

% Start the process.
start :-
    ask_preferences_and_filter().