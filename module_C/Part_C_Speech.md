# Part C — three-minute speaking script

**Presenter:** ZhangJingchen (2430026269)  
**Slides:** Part C opener, C1, C2, C3  
**Target:** about 3 minutes at a natural pace

## Slide 1 — Safety and rail (0:00–0:25)

After housing and noise, I add two renter concerns: reported incidents and rail access. I join 2024 NYPD complaints, ACS population, census tract boundaries and MTA stations. This lets us compare measures on the same map. We have 2,325 tracts with rail distance and 2,211 with eligible complaint rates.

## Slide 2 — C1: NYPD reports (0:25–1:10)

First, where is the complaint rate higher? I calculate reports per 1,000 residents for tracts with at least 500 residents. The dashboard map locates high-rate tracts. Here, bars compare borough medians and 100% stacks compare offense levels. The Bronx median is about 74, versus 36 on Staten Island. But Manhattan's middle half runs from roughly 38 to 117, so the dashboard also has a boxplot. Position helps us compare rates, while stacked color shows composition. These are recorded complaints, not individual risk.

## Slide 3 — C2: rail access (1:10–1:55)

Next, I measure straight-line distance from each tract to its nearest rail station complex, including Staten Island Railway. Within 0.8 kilometres are 94% of Manhattan tracts, but only 42% in Queens and 23% on Staten Island. The ranking identifies remote tracts; Staten Island tract 032300 is about 8.45 kilometres away. The dashboard map locates them, while cumulative curves allow any threshold. Distance is a useful proxy, though walking routes and train frequency need separate data.

## Slide 4 — C3: within-borough comparison (1:55–3:00)

Finally, do tracts near rail have different complaint rates? I compare four distance bands *within each borough*, so borough-wide differences do not dominate. Lines show the band medians; bars highlight the nearest-versus-farthest gap. In the Bronx, the nearest band has about 88 reports per 1,000, versus 39 in the farthest band. Brooklyn and Queens also have higher medians near rail. Staten Island changes little. Manhattan has only one tract in the farthest band, so I omit that comparison. Commuters, visitors and reporting behaviour may affect these rates; this is descriptive, not causal. Together, complaint location, offense mix, population and rail distance give us a fuller view of the same tracts. That leads to the next question: what differs when rent is similar?

## Source and interpretation notes

- NYPD historic complaint data: 2024 incident locations and offense levels.
- ACS 2020–2024 five-year estimates: tract resident population.
- MTA station complexes snapshot: closest rail station coordinates.
- NYC 2020 census tract boundaries: geographic join and representative tract points.
- Rates summarize recorded events per 1,000 *residents*; incident locations, visitors and daytime populations can affect them. Rail distance is straight line, not walking time.
