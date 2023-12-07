import csv
import re
from collections import defaultdict

def sanitize_string(s):
    # Remove backslashes, double quotes, and single quotes
    s = re.sub(r'[\\\'"]', '', s)
    # Replace other special characters with a space or specific character as needed
    s = re.sub(r'[^\w\s]', ' ', s)
    return s.strip()

def process_movie_title(title):
    # Handle special case where "The" comes after a comma
    if ',' in title:
        parts = title.split(',')
        if len(parts) == 2:
            title = parts[1].strip() + ' ' + parts[0].strip()

    # Extract year and clean title
    match = re.search(r'\((\d{4})\)', title)
    if match:
        year = match.group(1)
        title = sanitize_string(re.sub(r'\(\d{4}\)', '', title).strip())
        return title, year
    return sanitize_string(title), None

def read_movies(file_path):
    movies = {}
    with open(file_path, 'r', encoding='utf-8') as file:
        reader = csv.reader(file)
        next(reader)  # Skip header
        for row in reader:
            movie_id, title, genres = row
            if genres != "(no genres listed)":
                title, year = process_movie_title(title)
                if year:  # Only include movies with a known year
                    genres = genres.split('|')
                    movies[movie_id] = {'title': title, 'year': year, 'genres': genres}
    return movies

def read_ratings(file_path):
    ratings = defaultdict(list)
    with open(file_path, 'r', encoding='utf-8') as file:
        reader = csv.reader(file)
        next(reader)  # Skip header
        for row in reader:
            user_id, movie_id, rating, _ = row
            ratings[movie_id].append(float(rating))
    return {movie_id: sum(rates) / len(rates) for movie_id, rates in ratings.items()}

def read_tags(file_path):
    tags = defaultdict(set)
    with open(file_path, 'r', encoding='utf-8') as file:
        reader = csv.reader(file)
        next(reader)  # Skip header
        for row in reader:
            user_id, movie_id, tag, _ = row
            tags[movie_id].add(tag.strip())
    return tags

def generate_prolog_facts(movies, ratings, tags):
    facts = []
    for movie_id, movie in movies.items():
        if movie_id in ratings:
            title = sanitize_string(movie['title'])  # Sanitize title
            genres = ', '.join(f'"{sanitize_string(genre)}"' for genre in movie['genres'])
            rating = ratings[movie_id]
            movie_tags = ', '.join(f'"{sanitize_string(tag)}"' for tag in tags.get(movie_id, []))
            fact = f'movie("{title}", {movie["year"]}, [{genres}], {rating}, [{movie_tags}]).'
            facts.append(fact)
    return facts


# File paths
movies_file = 'movies.csv'
ratings_file = 'ratings.csv'
tags_file = 'tags.csv'

# Process files
movies = read_movies(movies_file)
ratings = read_ratings(ratings_file)
tags = read_tags(tags_file)

# Generate Prolog facts
prolog_facts = generate_prolog_facts(movies, ratings, tags)

# Write to a Prolog file
with open('movies.pl', 'w', encoding='utf-8') as file:
    for fact in prolog_facts:
        file.write(fact + '\n')