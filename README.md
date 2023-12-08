# Movie Recommendation System

This README provides instructions on how to set up and run the Movie Recommendation System on a Windows environment. This system is built using Prolog and utilizes a dataset of movies, their genres, tags, and ratings to provide movie recommendations.

## Prerequisites

Before running the system, ensure you have the following installed:
- SWI-Prolog: [Download and install SWI-Prolog](https://www.swi-prolog.org/Download.html) for Windows.

## Setup

1. **Clone or Download the Repository:**
   Ensure that you have the Prolog files in your working directory.

## Running the System

1. **Open Command Prompt:**
   Navigate to the folder containing your Prolog file (`movies.pl`).

2. **Start SWI-Prolog:**
   Run the command `swipl` to start the Prolog interpreter.

3. **Load the Facts File:**
   In the Prolog interpreter, load the facts file by running `[movies_prolog_facts].`

## Querying the System

You can query the system using various Prolog rules. Here are some examples:

1. **Find Movies by Genre:**
   ```prolog
   movie_by_genre("Adventure", Title).

## Interacting with the Movie Recommendation System
Once you have loaded the movies.pl file in the SWI-Prolog interpreter, you can interact with the movie recommendation system by following these steps:

## Start the Recommendation Process

Follow these steps to use the movie recommendation system:

1. **Initiate the Process**
   - Run the command `start.` in the Prolog interpreter to begin.

2. **Enter Your Preferences**
   - The system will ask for your preferences in the following order:

     - **Genre**: Type the genre you are interested in within double quotes (e.g., `"Comedy"`). Type `'any'` for no preference.
     - **Year**: Enter the desired year (e.g., `1995`). Type `'any'` for any year.
     - **Minimum Rating**: Enter the minimum rating (1 to 5). Type `'any'` for any rating.
     - **Tags**: Type a specific tag within double quotes (e.g., `"family"`). Type `'none'` for no preference or if the tag does not match.

3. **View Recommendations**
   - The system will display a list of recommended movies based on your input.

4. **Adjustments and Notifications**
   - If the system adjusts the criteria (like lowering the rating or changing the genre), it will notify you of these changes.
   - Adjustments are made as follows:
     - If no tags match, the system will search again without the tags.
     - If there are still no matches, the rating is lowered by 1 point, and the system searches again.
     - If no matches are found, the system will use the `similar_genre` rule to search based on a similar genre.

**Note**: You can test verify this easily by using the `simple.pl` file. It contains a smaller dataset of movies to test the prolog rules.

## Sample Output
```
?- [movies].
true.

?- start.
What genre are you interested in? (Type 'any' for no preference)
|: "Comedy".
What year are you interested in? (Type 'any' for no preference)
|: 1995.
What minimum rating are you looking for? (Type 'any' for no preference)
|: 5.
Any specific tags you're interested in? (Type 'none' for no preference)
|: none.
Adjusting rating criteria to find matches...
Movies recommended for you: 
Wallace   Gromit  A Close Shave (1995)
The Saint of Gamblers (1995)
Prehysteria  3 (1995)
Dana Carvey  Critics Choi
```

## Exit the System
To exit the Prolog interpreter, type halt. and press Enter.
