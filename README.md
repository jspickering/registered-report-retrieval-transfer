# Data and code for the Registered Report "Retrieval practice transfer effects for multielement event triplets"

OSF: https://osf.io/bgm3p/

Each script should be run in the following order:
* process_data_s1a.R
    - starts processing the data from Session 1
    - data from the exit questionnaire was then rated before starting the next script
* process_data_s1b.R
    - continues processing the Data
    - data from the retrieval practice trials was then rated by two researchers before starting the next script
* process_data_s1c.R
    - a subset of the rated retrieval practice data needed to be rated by a third researcher
* process_data_s1d.R
    - after verifying the accuracy, we were able to determine eligibility to move forward to Session 2
* process_data_s2a.R
    - starts processing the data from Session 2
    - data from the exit questionnaire was then rated before starting the next script
* process_data_s2b.R
    - finishes processing the data from Session 2
    - creates graphs
    - runs statistical analyses

Note that the qualitative data from the exit questionnaires does not form part of the shareable dataset and so the content of these questions has been deleted.

Credit for the raincloud plot code goes to https://github.com/RainCloudPlots/RainCloudPlots

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
