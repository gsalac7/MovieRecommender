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
