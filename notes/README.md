# 8 July 2025 

Some updates on the project: 

- I think the spatial transmission model is no longer necessary. I just can't see us using it to meaningfully say anything about the timing and location of introductions, since it's not clear that a future influenza pandemic will spread like the last one. But, I do think that the local-level model could still be useful, and there's still a geographic analysis to be done. We should just look at the impact on food production of an outbreak in various counties, at different times of year. 
- It would be nice, too, to incorporate some of the information from Zia's FoodTwin project. Beyond the impact on food production, what might be the impact on downstream food availability? Basically, adding a disease layer to their map. 

I want to try to code up the beta parameterization independently to see if we get the same thing, and to make sure I understand exactly what's happening. We should also look at how variable R0 is across locations using this method. 

# 14 July 2025 

I think an interesting approach here might be to try to impute the household crowding and proportion-under-18 in ag workers at the county level using ACS and NAWS data using a sort of pooled Bayesian approach. That might be more credible than a simple weighting scheme, or simply using the regional NAWS to reflect the county-level ones. 

Before embarking on this, though, I need to make sure the county-level analysis is really how I want to do it. It seems like the right thing to do -- since food production is also measured at the county scale (see FoodTwin). It's a common scale for geospatial epi analysis, too. 

It looks like I can use [data from FoodTwin directly](https://github.com/earthrise-media/food-system-digital-twin/tree/main/input-data) to grab county-level food production, so that part shouldn't be too challenging. 

For the crowding and proportion under 18: we might want to list some sensitivity analyses to try. These might include: 

- Assume all ag workers are fixed at the regional NAWS value 
- Use the ACS 
- Use an adjusted ACS (start with agricultural worker-specific ACS values, but multiply by some constant so that it matches the regional NAWS values) 
- Use another adjusted ACS (assume variation in ag workers follows variation in the general community, and multiply by whatever you need to get the regional NAWS values)

Ideally these will show consistent or at least interpretable things. 

Ultimately, this will give us estimates of crowding and age structure for the agricultural and non-agricultural community. Then what? 

Given these values, we want to simulate disease spread. We need to tune (a) the degree to which R0 is impacted by these demographic variables and (b) the degree of assortativity between agricultural workers and the general community. These will probably be sensitivity analyses. 

With this, we should be able to simulate outbreaks in the general community and in agricultural workers. This, I think, will be the bulk of the project. The rest will consist of interpreting the results of these simulations. 

For example, we might want to first describe detection lags: when we notice a major outbreak in a community, how far has the outbreak in agricultural workers already progressed? How is this exacerbated by gaps in surveillance? 

What's the total impact (in terms of food production and GDP) of an influenza outbreak? Which locations contribute most to losses? How does this depend on time of year? How might mitigation help? 


# 15 July 2025 

We'll also probably want to include a list of recommendations for data to collect/things to pay attention to/things that would modulate the impact during a pandemic. For example, the seasonality of the virus might also be important (a winter-time outbreak might be more severe epidemiologically, though maybe less disruptive agriculturally) 

It might make sense to use a variant of [House & Keeling's 2009 household-structured transmission model](https://pubmed.ncbi.nlm.nih.gov/18840319/)

Also see [this](https://tah-sci.com/house.pdf) for more detail


Odin functions: https://cran.r-project.org/web//packages/odin/vignettes/functions.html










