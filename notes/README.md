# 8 July 2025 

Some updates on the project: 

- I think the spatial transmission model is no longer necessary. I just can't see us using it to meaningfully say anything about the timing and location of introductions, since it's not clear that a future influenza pandemic will spread like the last one. But, I do think that the local-level model could still be useful, and there's still a geographic analysis to be done. We should just look at the impact on food production of an outbreak in various counties, at different times of year. 
- It would be nice, too, to incorporate some of the information from Zia's FoodTwin project. Beyond the impact on food production, what might be the impact on downstream food availability? Basically, adding a disease layer to their map. 

I want to try to code up the beta parameterization independently to see if we get the same thing, and to make sure I understand exactly what's happening. We should also look at how variable R0 is across locations using this method. 

# 18 July 2025 

I think an interesting approach here might be to try to impute the household crowding and proportion-under-18 in ag workers at the county level using ACS and NAWS data using a sort of pooled Bayesian approach. That might be more credible than a simple weighting scheme, or simply using the regional NAWS to reflect the county-level ones. 

Before embarking on this, though, I need to make sure the county-level analysis is really how I want to do it. It seems like the right thing to do -- since food production is also measured at the county scale (see FoodTwin). It's a common scale for geospatial epi analysis, too. 

It looks like I can use [data from FoodTwin directly](https://github.com/earthrise-media/food-system-digital-twin/tree/main/input-data) to grab county-level food production, so that part shouldn't be too challenging. 
