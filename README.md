# Flight-pricing-and-low-cost-airlines

This is a model where different airlines compete in price on a given route. We observe what are the consequences of the entry of a low cost airline and, more precisely, how the airfares of the incumbent change.

The two situations provided in the R documents are:
* Two Full Service Carriers (FSC) with different initial market shares (A has 60% of the market and B has 40%) compete on a route
* Three airlines, two FSC and one Low Cost Carrier (LCC), compete

To run this simulation, we used a genetic algorithm. 
Each solution (which is a flight schedule and a price) is represented by a chromosome (vector made of 0 and 1). 
We generate a population of solutions associated with each airlines, do cross-overs and mutations to try to improve our solutions, then select the better solution (the one that maximizes the profit of the company). During one game, each airline one by one adjusts its price and its schedule to maximize its profit. 
At the end, we come to an equilibirum, where no airline can increase its profit.

More information about the algorithm and the method can be found in a paper by Ko and Hwang (_Management Strategy of full-service carrier and its subsidiary low-cost carrier_, 2011)

Below, we provide three results. The first one comes from the scripts that are provided, and the second and third one are derived from the same simulation with
* changes of parameters of B (second slide)
* an additional airline (LCC) is added (third slide)

![Alt text](/Three insights/Slide1.jpg?raw=true "Optional Title")
![Alt text](/Three insights/Slide2.jpg?raw=true "Optional Title")
![Alt text](/Three insights/Slide3.jpg?raw=true "Optional Title")
