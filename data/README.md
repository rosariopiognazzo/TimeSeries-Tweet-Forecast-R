# Dataset Information

## Why the CSV is not included

The raw tweet data cannot be redistributed in this repository. Sharing full tweet content, including usernames, tweet text, and associated metadata, is prohibited under the [Twitter/X Developer Agreement and Policy](https://developer.twitter.com/en/developer-terms/agreement-and-policy).

---

## Original Dataset

**File:** `Sentiment_fr_tweet_2023.csv`  
**Required location:** repository root (same level as this `data/` folder)

The dataset was collected via the Twitter Academic Research API. It contains French-language tweets related to the Russia-Ukraine war, posted between 24 February and 4 March 2023, filtered using relevant hashtags.

**Dimensions:** 112,958 rows x 19 columns

### Variable Descriptions

| Variable | Type | Description |
|---|---|---|
| `userid` | char | Unique identifier of the user who posted the tweet |
| `following` | numeric | Number of accounts the user follows at extraction time |
| `followers` | numeric | Number of followers the user has at extraction time |
| `totaltweets` | numeric | Total tweets posted by the user at extraction time |
| `tweetid` | char | Unique identifier of the tweet |
| `tweetcreatedts` | POSIXct | Timestamp when the tweet was posted |
| `retweetcount` | numeric | Number of retweets received at extraction time |
| `favorite_count` | numeric | Number of likes received at extraction time |
| `is_retweet` | bool | TRUE if the post is a retweet |
| `original_tweet_id` | char | ID of the original tweet (if retweet) |
| `original_tweet_userid` | char | ID of the original tweet's author (if retweet) |
| `in_reply_to_status_id` | char | ID of the post being replied to |
| `in_reply_to_userid` | char | ID of the user being replied to |
| `is_quote_status` | bool | TRUE if the post contains a quote tweet |
| `quoted_status_id` | char | ID of the quoted post |
| `quoted_status_userid` | char | ID of the quoted user |
| `extractedts` | POSIXct | Timestamp when the data was extracted |
| `sentiment` | categorical | Sentiment label: `neg`, `neu`, or `pos` |
| `score` | numeric | Quantitative sentiment score (range: 0.34 to 0.97) |

Note: Because tweets were extracted at multiple points in time, the same `tweetid` or `userid` may appear more than once with different values for time-varying fields (e.g. `retweetcount`, `favorite_count`).

---

## How to Place the Dataset

Save the CSV file to the repository root so that the relative paths used in the R Markdown notebooks resolve correctly:

```
TSAnlysis-Tweet/
└── Sentiment_fr_tweet_2023.csv   <-- place the file here
```

The notebooks load the dataset with:

```r
read_csv2("../Sentiment_fr_tweet_2023.csv")
```

---

## Synthetic Datasets

Two synthetic datasets were generated using ChatGPT, based on the statistical summary of the original dataset (means, standard deviations, distributions, Pearson correlations, and frequency tables).

| File | Description |
|---|---|
| `dataset_sintetico_small.json` | Reduced synthetic dataset |
| `dataset_sintetico_large.json` | Full-scale synthetic dataset |

To generate your own synthetic dataset, use the prompt in `Progetto_Gnazzo_Pauzano/PROMPT-SAD.txt` with any capable language model and save the output as JSON in this folder.
