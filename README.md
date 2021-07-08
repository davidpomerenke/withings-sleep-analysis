# Withings Sleep Analysis

[`Withings Sleep (Analyzer)`](https://www.withings.com/nl/en/sleep-analyzer) is the most accurate sleep tracker for people with [Narcolepsy](https://www.nhs.uk/conditions/narcolepsy/). The dashboard in the app, however, does not properly incorporate naps. This project is an attempt to build a dashboard which does. 

Unfortunately, it turns out that the app does not export all relevant data. For example, information about the time of breaks during sleep periods is shown in the app, but cannot be reconstructed from the exported data. Work on this project is therefore on hold.

In addition to the export functionality, there is also an [API](https://developer.withings.com/developer-guide/) by Withings. Future work could explore whether the API does provide all relevant data. [Struggles with the API](https://gist.github.com/katemonkeys/e17580777b57915f5068) have been documented.

## Usage

1. Go to the Withings Health Mate app ([Android](https://play.google.com/store/apps/details?id=com.withings.wiscale2) | [iOS](https://apps.apple.com/us/app/withings-health-mate/id542701020)), `Profile` → `⚙` → `Download your data` → `Start my archive`.

2. With a delay of a few minutes, you will receive a mail with a link to download the Zip archive. Download it.

3. Go to [`withings-sleep-analysis.surge.sh`](https://withings-sleep-analysis.surge.sh) and upload the downloaded Zip archive there. Your data will be processed locally in your browser and will not be stored.

## Development

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).
