import nltk
from nltk.corpus import brown
import pandas as pd
from nltk.corpus import wordnet as wn

nltk.download('brown')


# Function to calculate adjectivalness for a given word
def calculate_adjectivalness(word):
    tagged_words = brown.tagged_words()

    # Initialize counts
    total_count = 0
    article_noun_count = 0

    # Iterate through the tagged words
    for i in range(len(tagged_words)):
        # Check if the current word matches the word in the list
        if tagged_words[i][0].lower() == word.lower():
            total_count += 1

            # Check if next word is a noun (NN)
            if i > 0 and i < len(tagged_words) - 1:
                if tagged_words[i + 1][1] == 'NN':
                    article_noun_count += 1

    # Calculate adjectivalness
    adjectivalness = article_noun_count / total_count if total_count > 0 else 0

    return total_count, article_noun_count, adjectivalness


# Read the 'keep' Excel file
keep_df = pd.read_excel('keep.xlsx')
print(keep_df['ing_form'])

# Extract words from the 'Verb' column
ing_forms = keep_df['ing_form']
# Remove duplicates by converting the list to a set and then back to a list
words_to_lookup = set(ing_forms)
words_to_lookup = list(set(words_to_lookup))
print(words_to_lookup)

# Initialize lists to store results
results = []

# Iterate over each word in the list
for word in words_to_lookup:
    total_count, article_noun_count, adjectivalness = calculate_adjectivalness(word)

    # Append results to the list
    results.append([word, total_count, article_noun_count, adjectivalness])
    print("Word:", word)
    print("Total occurrences of", word + ":", total_count)
    print("Occurrences of", word + " between an article and a noun:", article_noun_count)
    print("Adjectivalness:", adjectivalness)
    print()

# Create a DataFrame from the results list
df = pd.DataFrame(results, columns=['Word', 'Total Occurrences', 'Occurrences after Noun', 'Adjectivalness'])

# Save DataFrame to an Excel file
df.to_excel('adjectivalness_analysis.xlsx', index=False)
