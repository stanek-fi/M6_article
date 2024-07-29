This is a replication repository for the article *Designing Time-Series Models With Hypernetworks & Adversarial Portfolios*.
To replicate the analysis:

1. Install dependencies in `requirements.txt`
2. Run the analysis via either:
    -  If you are a [DVC](https://dvc.org/) user, simply run `dvc exp run` in the console or run the `run.bat` file.
    -  If not, you can also run all the files manually in the following order:
        1. `src\m6\portfolio_weights.R`
        2. `src\m6\quintile_predictions.R`
        3. `src\m6\scaling_effects.R`
        4. `src\sinusoidal\sinusoidal.R`
        5. `src\m4\m4.R`
        6. `src\m4\m4_postprocessing.R`
3. To create the pdf, compile the files:
   1.  `text\article_arxiv\tikz\diagram.tex`
   2.  `text\article_arxiv\tikz\diagram_specific.tex`
   3.  `text\article_arxiv\article_arxiv.tex`

