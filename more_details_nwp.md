
### Limitations
1. **UNPREDICTABLE next word:** If the next word is not predictable, it would not display any word. It will be blank. But displays a message. Sometimes, even if it is a valid word, it may not be able to predict.
2. **PUNCTUATIONS in corpus and search string:** The punctuations are stripped off. So, predicted word which may need a punctuation may not have one. Like Jr. it would display only "Jr"
3. **SPELLING ERRORS in predicted word:** There could be some spelling mistakes in predicted words. Spell checks are part of the next phase, due to limited time for this project.
4. **OVERLAP of predicted words:** Sometimes, in the 3 words section, some words do overrun the space alloted, overlaps with other. Usually, words more than 7 characters.
5. **NO SENTENCE BOUNDARY:** When you give 2 sentences, it takes the last set of words as one sentence. This is again a limitation for now which will be taken-up next phase.
6. **CONTINUOUS ONE WHOLE SENTENCE:** Sometimes while predicting, even if a sentence ends, it continues to predict if there is a relevant match in the lesser grams. This is a current limiation, this will be taken up as part of next phase.
7. **TWITTER NO RESULTS at times**: Authetication is required to get twitter results, thus, if there is any issue with authentication, it may not return results.
<hr/>

### REFERENCES   
1. The CSS used in these are with references to http://bootswatch.com/
2. The commenting style has been reffered to https://github.com/rstudio/shiny-examples/blob/master/065-update-input-demo/server.R
3. https://github.com/rstudio/shiny-examples
4. https://journal.r-project.org/archive/2013-1/collingwood-jurka-boydstun-etal.pdf
5. http://web.mit.edu/6.863/www/fall2012/lectures/lecture2&3-notes12.pdf
6. https://cran.r-project.org/web/packages/NLP/NLP.pdf
