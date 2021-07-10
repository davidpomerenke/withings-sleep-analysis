# Withings Sleep Analysis

[`Withings Sleep (Analyzer)`](https://www.withings.com/nl/en/sleep-analyzer) is the most accurate sleep tracker for people with [Narcolepsy](https://www.nhs.uk/conditions/narcolepsy/). The dashboard in the app, however, does not properly incorporate naps. This project is an attempt at building a dashboard which does.

## Usage

1. Go to the Withings Health Mate app ([Android](https://play.google.com/store/apps/details?id=com.withings.wiscale2) | [iOS](https://apps.apple.com/us/app/withings-health-mate/id542701020)), `Profile` → `⚙` → `Download your data` → `Start my archive`.

2. With a delay of a few minutes, you will receive a mail with a link to download the Zip archive. Download it.

3. Go to [`withings-sleep-analysis.surge.sh`](https://withings-sleep-analysis.surge.sh) and upload the downloaded Zip archive there. Your data will be processed locally in your browser and will not be stored.

## Development

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).
