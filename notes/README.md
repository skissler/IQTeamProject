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

Also see [this](https://tah-sci.com/house.pdf) for more detail on the model. 

Odin functions: https://cran.r-project.org/web//packages/odin/vignettes/functions.html

Also: UC Davis has some really nice files estimating the cost of producing various crops: https://coststudies.ucdavis.edu/current/commodities

For this, I want to use [strawberries](https://coststudyfiles.ucdavis.edu/2024/04/04/2024Strawberry-FULL-FINAL-March2024.pdf), [lettuce](https://coststudyfiles.ucdavis.edu/uploads/pub/2023/08/04/2023-iceberglettuce-full-final.pdf), and [oranges](https://coststudyfiles.ucdavis.edu/uploads/pub/2021/08/12/2021orangessjvsouth.pdf). For strawberries, there's a table shownig percent harvested by time; for the others, there are verbal descriptions, and there's an estimate of expenditure by month broken down by activity (including harvesting) so we can triangulate harvesting periods for those, too. I think these are interesting because: 

* Strawberies are super labor-intensive and have a relatively long summer harvest season
* Lettuce is also pretty work-intensive and has potentially two harvesting periods, one in April and one in July (planting in late Dec/early Jan, harvests are separated by about 100 days) 
* Oranges have three winter-time harvests for three different types of orange. They're probably less labor-intensive but still require some hand-picking. 

So, I think this is getting somewhere: next steps include: 

* Testing the household model to make sure it's giving me sensible things
* Testing that we can get different types of epidemics out when we stratify by household size 
* Put in the estimated household sizes by county, for the general community and for agricultural workers, and simulate some outbreaks (this will require estimating some parameter values -- tuning them to get the right reproduction number and secondary attack rates, as House and Keeling do in their paper) 
* Run some analysis: look at extent of spread in the agricultural community upon detection in the general community and quantify the detection/surveillance gap 
* Estimate production losses by county and overall for an epidemic 
* Do a deeper dive into productivity losses for strawberries, lettuce, and oranges, highlighting how time of year modulates the impact of a pandemic. 

I think that's enough for the analysis. 

# 16 July 2025 

Ok -- there's some difference between high-occupancy households and low-occupancy households, which scales with the degree of within- vs. between-household transmission. The difference isn't huge, though; that said, I think that assortativity could also play a big role here. If two populations are fully assortative (and if they're similarly sized, etc) we expect their epidemiological dynamics to be fairly similar. I expect that even a little assortativity will substantially separate out the curves, though, if we have one population with higher crowding on average than the other. So, my next step (after maybe doing a bit more validation of the household model) is to incorporate assortativity into a two-population household-structured model. 

First thing: I'm going to tie up the code into a better set of packages. 

Ok -- I just coded up a basic SIR model, and when my household sizes are 1 and tau is 0, they end up with identical outputs, as expected. Still some testing to do to make sure the household-specific stuff is working as it should be, but I think this is nearly there. 

Alright -- got the two-population model working, and it's looking pretty good. With 75% assortativity, and with a household size of 4 in the community and 8 among agricultural workers, we see a clear higher and earlier peak in the ag workers. This remains true even when assortativity is proportional to population sizes, though of course the effect attenuates some. I've just added in an option to vary tau and beta between the populations. We might be able to parameterize those useing secondary household infection rates (tau) and, possibly, by making some basic assumptions about elevated transmissibility/susceptibility based on systematic differences in age structure, though that'd be a lot more conjectural. 

# 18 July 2025 

For today: incorporating the actual crowding information for ag workers vs. the general population. Start with a single county, or maybe with the overall national averages; then repeat by county. 

Also, what should I be using for beta and tau? 

I'm working on importing the household size data now -- it might end up being similar across the groups, in which case focusing on crowding could make more sense. there might be a way to incorporate a notion of crowding into a house/keeling-type model

How does crowding impact transmission? 

- 3x (1-10) odds of transmission (https://arc.net/l/quote/fifxwuqo)
- Household crowding may be more important than number of people per household (https://arc.net/l/quote/aiafekoe)
- Household crowding strongly associated with influenza incidence (https://arc.net/l/quote/oekioxjq)
- 2-3x higher influenza hospitalization incidence in communities with high crowding vs low crowding (https://arc.net/l/quote/mqkvymfc)
- 1.17x higher influenza incidence in crowded households here (https://arc.net/l/quote/swgtrdas)
- here, crowding maybe 2x risk of influenza-related hospitalization (https://arc.net/l/quote/twfnjxis)
- the relative rate in high-crowding vs low-crowding census tracts ranged from 1.9 in the 2008–2009 season to 8.6 in the 2006–2007 season. (https://arc.net/l/quote/onbezqtz)
- Here they don't find a link with crowding, but sizes are small: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0075339#pone-0075339-t002
- maybe 50% SAR for SARS-CoV-2 in high-density settings, compared to 24% in non-high-density settings? (https://arc.net/l/quote/khvephkh) - or maybe a better comparison is 30% vs 50%. https://arc.net/l/quote/qngfvfuo


Say 2x risk of influenza transmission in crowded households? Something like this? How to translate this into a revised tau that accounts for crowding? 

House and Keeling use a 40% SAR (seems high?) and a reproduction number of 2 for their model. How exaclty does this work? 

Approach: start with mean household size distribution of the US overall. Start with tau = 1/4 gamma (gives SAR of 20%). Find the beta that gives us the reproduction number we want by comparing with a simple SIR model (or getting out the right overall final size). 

Then, this gives us the beta and tau values for going forward. We'll want to use the same beta values for everyone, I think. Then, we can consider using different tau values for crowded households. (We might want to include national-level household crowding in our original analysis, too). 

A note on calculating tau from the SAR: the idea is simply that the SAR is equal to tau / (tau + gamma). If we express tau as a multiple of gamma (i.e. tau = k gamma) and simplify, then we find that k = SAR / (1-SAR). So, a 20% SAR gives tau = 1/4 gamma; a 40% SAR gives tau = 2/3 gamma; a 50% SAR gives tau  = gamma. I think we might want to range tau from 1/4 gamma to gamma, to range from a 20% to 50% SAR, with maybe tau = 2/3 gamma (40% SAR) as our baseline for crowded households. 

A key issue is that we don't have the information on the relationship between household size and household crowding. We can probably start with the assumption that households are crowded uniform-randomly. Note that the census defines crowding as whne the number of people per room exceeds 1, so only households of size 2 and greater can be crowded. Then, we can maybe do a sensitivity analysis where larger households are more likely to be crowded. This will exacerbate the effect of crowding on the epi outcomes, I think; so the uniform random case will be a conservative lower bound. 

Alright, so here's a calibration approach, in more detail: 

- For each county, pull the household size distribution and the percent crowding. 
- Assign crowding to households proportional to the frequency of household sizes 
- Assign secondary attack rates for regular and crowded houses according to the household crowding index 
- Simulate epidemics; vary beta to get the right reproduction number (final size) 
- Then, repeat the analysis stratifying by agricultural workers and non-agricultural workers. 

This might require further stratifying the transmission model into community/agricultural and crowded/not crowded... 

Ok -- seems like the two-population crowding model is working reasonably well, too. Next: incorporating some data. 

# 19 July 2025 

I might start here by cleaning up the code. I'm in a good spot with the models, I think; I want it to be relatively streamlined when I incorporate the data and work on fitting. 

Ok -- got the crowding variables imported now, too, and confirmed that it looks like crowding is a lot higher among agricultural workers than the general population. Some figures to make on this still; but for now, I think next steps are to try to incorporate this information into a transmission model. 

Maybe an important thing to do next is to extract the number of agricultural workers by county. 

That's done now too. 

I think the next thing is to start with a national average: using the national average for household sizes and crowding (and secondary infection rate), figure out some sensible beta, tau, and tau_boost values. Then, apply these at the county level for some projections. 

To prepare, I'm going to: 

- Clean up the acs imports 
- Clean up the naws imports 
- Create a data frame with county-level data including percent crowding, household size distribution, split between ag and non-ag workers, with raw and adjusted values... I think? 

Now, something to consider: do I need to somehow adjust the acs data to reflect the proportion of agricultural workers/households when I'm doing the calibration? Probably -- since there are some counties with well over 20% of the population involved in agriculture. 

So, a strategy: 

Using the ACS data, we can compute a national population-weighted average distribution of household sizes and level of crowding. Maybe start there. Then, given this national average, find a reasonable beta, tau, and tau_boost. 

Once I have those values in hand, the task will be to simulate epidemics at the county level. This is where I'll have to carefully separate out agricultural and non-agricultural workers/households. For the agricultural workers, I should just be able to use the NAWS data (possibly adjusted to reflect differences in county-level household sizes and crowding levels; but this will be a follow-on analysis). For the "community" data, I'll need to somehow subtract out the effect of the agricultural workers. so, for whatever values I decide to use for the agricultural workers, I'll need to correct the ACS data so that we get the right overall ACS values by combining the "community" and the ag worker values. 

Ok -- I now have a useful function, `adjust_crowding()`, which creates an adjusted proportion of crowded households across the different household sizes. It ensures that the proportion of crowded households of size 1 is zero (a household of size 1 can't be crowded by definition), and then distributes the remaining crowding among the households of other sizes. You can adjust the fold-difference between crowding in a 2-person household vs a 7+-person household, so that they're the same (i.e. all households sizes 2+ have the same fraction crowded) or, for example, maybe it's twice as likely for a 7+-person household to be crowded vs a 2-person household. The likelihood of crowding increases linearly across household sizes. I've confirmed that we can crank the fold-difference up to 5 and still get valid probabilities out (it'd probably be best to make it somehow nonlinear so that we always get out valid probabilities, but this should work for now). 

With the data -- I think it's mostly going to change the initial conditions, i.e. the distribution of households across sizes and crowding levels. 

Alright -- I've got a first pass working for a national model. I'm going to commit this, then will do some cleaning. 

Here are some parameter values that give me a 20% SAR for non-crowded households, 40% SAR for crowded households, and a reproduction number of 2 (assuming everything's working like I think it is): 

```

# Initialize model
mod_twopop_crowding <- household_model_twopop_crowding$new(
  n_states = n_states,
  x = household_states$x,
  y = household_states$y,
  z = household_states$z,
  hh_size = household_states$hh_size,
  crowded = household_states$crowded,
  rec_index = household_states$rec_index,
  inf_index = household_states$inf_index,
  init_C = init_nat_C,
  init_A = init_nat_A,
  gamma = 1/5,
  tau_C = (1/4)*(1/5), # 20% SAR
  tau_A = 0, 
  tau_boost = (2/3) - (1/4), # Boosts to a 40% SAR 
  beta_C = 1.52*(1/5), 
  beta_A = 0,
  eps = 0,
  pop_C = 1000,
  pop_A = 0
)

```

Now, I think, the next thing is to figure out how to run simulations at the county level. I still need to figure out how to extract the agricultural worker data to get separate non-ag and ag information on household sizes and crowding. 

# 20 July 2025 

Extracting the agricultural worker data probably isn't a terrible idea -- but I realized last night that if we *don't* extract them, we'll end up with a more conservaive estimate of the difference between ag workers and the general community, since the community will be more similar to ag workers than it would be otherwise (if we did do the extraction). Because of this, I think it's not worth doing the extraction, at least for our initial analysis. We can always return to this; but for now it makes sense to focus on other things. 

Like varying household sizes and crowding for ag workers by county -- that seems less than straightforward. Again, for the initial analysis, we should assume that ag workers just follow the regional average; but I'd like to also adjust the ag workers' data to reflect county-specific averages. How should we do this? 

Ok - it seems like I have a basic simulation working comparing county-level ag workers and general community transmission. The difference isn't massive, but it's there. I need to figure out how to automate this; then, I can look at comparing the curves more rigorously. 


# 21 July 2025 

I realized last night that an assortativity parameter of epsilon = 0.8 may still be way too high -- i.e. way too much proportional mixing. Agricultural workers on average make up 4.7% of the workforce of a given county. To calculate the fraction of contacts that agricultural workers have with other agricultural workers, we need: 

(1-epsilon) + .047*(epsilon)

So, a table: 

| epsilon | fraction of contacts  | 
-----------------------------------
|   0     |          1            |
|   0.1   |          0.90         |
|   0.2   |          0.80         |
|   0.5   |          0.52         |
|   0.8   |          0.23         |
|   1     |          0.047        |

This all makes sense -- basically, since the agricultural population is small relative to the general community, epsilon gives roughly the number of contacts that an agricultural worker has with other agricultural workers. For the general community, even when epsilon is 1 (fully proportionate mixing), over 95% of contacts are still with the general community. So, changing epsilon won't really change the dynamics of the general community -- we're working with between 95% and 100% of contacts being among other general community members. It really just varies the number of contacts agricultural workers have with other agricultural workers, and if we think that most contacts are assortative - which I think is reasonable -- then we actually want epsilon to be relatively small; almost certainly something less than 0.5. 






























