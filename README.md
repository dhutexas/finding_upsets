# finding_upsets
This is a GitHub repository to accompany the Medium article on visualizing model components to understand uncertainty in statistical models. (https://towardsdatascience.com/can-we-see-an-upset-before-it-happens-predicting-the-madness-of-march-b16e89d972ec?sk=56d771d8bff9070212ab158a937dd8b0)

engineer_data.R contains the code necessary for constructing the data utilized in the paper. Methods to collect, join, and tidy the data are included, as are custom calculations to develop a point estimate and confidence intervals for each team.

plots.R contains the custom theme for the plots in the article along with a function to create them from the specific dataset for the paper.

The Data folder contains all of the necessary files (in csv format), including a script to gather the Massey ratings from the internet.
