% Untangling the link between traits, size and and growth rate in plants
% Daniel Falster; Rich FitzJohn; Karou Kitajima; Joe Wright

# Abstract

* The well documented trade-off between leaf-construction cost ($\phi$) and leaf turnover rate, known as the leaf economics spectrum, represents a core difference in the strategy plants use to generate and invest surplus energy[@wright_world-wide_2004; @falster_influence_2011].
* In seedlings, $\phi$ correlates tightly with plant growth[@cornelissen_seedling_1996; @wright_cross-species_2000]; the strength of the relationship leading many to use $\phi$ as a generic indicator of potential growth rate. Although existing models suggest this relationship should extend to larger plants[@enquist_general_2007], several attempts to link $\phi$ with growth rate in large trees have been unsuccessful[@wright_functional_2010, @herault_functional_2011, @poorter_are_2008].
* Here we use a mechanistic model based on carbon balance and allocation[@falster_influence_2011] to first explain why the link between leaf-construction cost and growth rate breaks down in larger plants. The reason is that benefits of cheap leaf construction diminish at larger sizes, while the costs (increased leaf turnover rate) remain.
* We then use our model to generate ??4?? other fundamental predictions about plant growth and the influence leaf-construction cost; these predictions are verified using data from experimental plantings and long-term plot surveys of the tropical forest at Barro Colorado Island, Panama.
* Combined, out model ...

# Main text

Plant growth is the foundation of the terrestrial biosphere, powering food-webs and much of human activity[@raupach_carbon_2010]. While most higher plants have the same basic resource requirements (light water, nutrients) and physiological bauplan, large differences exist among species in the quality of construction and allocation to different tissues (leaves, stems, roots)[@westoby_plant_2002;@niklas_evolution_2000]. Leaf-construction cost ($\phi$) -- the amount of mass used to construct a unit of leaf area -- is one of the most prominent dimensions of variation, varying ~40 fold among species[@wright_world-wide_2004]. Low $\phi$ can, in principle, accelerate growth, as leaves which are cheap to build enable faster canopy expansion[@blackman_compound_1919; @westoby_time_2000]. Indeed, growth rate in seedlings is strongly and negatively correlated with $\phi$ (Fig. \ref{f-RGR}a)[@cornelissen_seedling_1996; @wright_cross-species_2000]. While it has been suggested that low $\phi$ might also give a growth advantage in larger plants[@enquist_general_2007], an increasing number of empirical studies have failed to detect any such effect in large trees [@wright_functional_2010, @herault_functional_2011, @poorter_are_2008] (Fig. \ref{f-RGR}c). The growing discord between theoretical expectation and empirical result thus indicates a strong need for a new theoretical framework through which the influence of $\phi$ on plant growth can be understood.

Here we extend a widely-used theoretical model used to link $\phi$ with growth rate in seedlings (refs) to explicitly include influences of size, light environment, and effects of leaf turnover, and thereby a number of general novel predictions on the link between plant demography and $\phi$ (Table \ref{tab:predictions}). The new model captures the observed change in correlation between $\phi$ and growth rate with increasing size (Fig. \ref{f-RGR}), and also between $\phi$ and shade tolerance (Fig. \ref{f-LMA_wplcp}). Moreover, we provide a novel explanation for the observed tendency of $\phi$ to increase through ontogeny (Fig. \ref{f-LMA_optim}). Together, these findings help to integrate a large number of empirical findings about plant growth and its link to traits[@wright_functional_2010, @herault_functional_2011, @poorter_are_2008] .

We begin with a standard model for the amount of biomass available for growth, $dP / \textrm{d}t$, given by the difference income (total photosynthesis) and losses (respiration and turnover) for the different plant components[@falster_influence_2011]:

\begin{equation}\label{eq:dPdt}
\frac{dP}{\textrm{d}t}= \underbrace{\strut Y}_\textrm{yield} \big(\underbrace{\strut \frac{m_\textrm{l}}{\phi} \, A}_\textrm{photosynthesis} - \big(\underbrace{\strut \, \frac{m_\textrm{l}}{\phi} \, r_\textrm{l} + \sum_\textrm{i=b,s,r}{m_\textrm{i} \, r_\textrm{i}}}_\textrm{respiration}\big)\big) - \big(\underbrace{\strut \sum_\textrm{i=l,b,s,r}{m_\textrm{i} \, k_\textrm{i}}}_\textrm{turnover}\big).
\end{equation}
Here, $m,r$, and $k$ refer to the mass, respiration rate, and turnover rate of different tissues with subscripts $l$=leaves, $b$=bark, $s$=sapwood and $r$=roots, while $A$ is the assimilation rate per leaf area and $Y$ is yield, which accounts for growth respiration (see Table \ref{tab:params} for units and definitions). Photosynthesis is proportional to leaf area, $a_\textrm{l} = m_\textrm{l} / \phi$, while total mass of living tissues $m_\textrm{t}=m_\textrm{l}+m_\textrm{b}+m_\textrm{s}+m_\textrm{r}.$ Eq. \ref{eq:dPdt} assumes that tissues lost via turnover are replaced before new growth occurs.

Current theory emphasises the relationship between $\phi$ and relative growth in mass[@enquist_general_2007], $R_{m_\textrm{t}}= \textrm{d}P / \textrm{d}t \, / \, m_\textrm{t}$. For seedlings, which are young and mostly leaf, it is reasonable to ignore all turnover terms as well as the respiration terms for non-leaf tissues. Net production then becomes a linear function of leaf area and net photosynthesis per leaf area ($A_\textrm{net} = Y(A - r_\textrm{l})$), making $R_{m_\textrm{t}}$ a linear function of $\phi$:
\begin{equation}\label{eq:RGR}
R_{m_\textrm{t}} \approx A_\textrm{net} \times \phi^{-1} \times \frac{m_\textrm{l}}{m_\textrm{t}}. \end{equation}
While Eq. \ref{eq:RGR} captures patterns of growth in seedlings
[@wright_cross-species_2000], this model is difficult to test with variables that are routinely collected for large trees: namely plant height ($h$) and stem cross-sectional area ($a_\textrm{st}$) or diameter $D$.

To properly model growth in either height or basal area growth requires one to account not just for mass production, but also for the costs of building new tissues, allocation to reproduction, and architectural layout. For height growth, this can achieved by decomposing $\textrm{d}h / \textrm{d}t$ into a product of physiologically relevant terms[@falster_influence_2011]:
\begin{equation} \label{eq:dhdt}
\frac{\textrm{d}h}{\textrm{d}t}= \underbrace{\strut\frac{\textrm{d}h}{\textrm{d}a_\textrm{l}}}_\textrm{architecture}
\times \underbrace{\strut\frac{\textrm{d}a_\textrm{l}}{\textrm{d}m_\textrm{t}}}_\textrm{leaf deployment rate}
\times \underbrace{\strut\frac{\textrm{d}m_\textrm{t}}{\textrm{d}P}}_\textrm{allocation to growth}
\times \underbrace{\strut\frac{\textrm{d}P}{\textrm{d}t}}_\textrm{mass production}.
\end{equation}

The first term on the RHS of eq \ref{eq:dhdt}, $\textrm{d}h / \textrm{d}a_\textrm{l}$, is the growth in plant height per unit growth in total leaf area; accounting for the architectural strategy of the plant. (Some species tend to leaf out more than grow tall, while other species emphasise vertical extension[@poorter_architecture_2006].)

The second term, $\textrm{d}a_\textrm{l} / \textrm{d}m_\textrm{t}$, accounts for the cost of deploying a unit of leaf area, including construction of the leaf itself and various support structures. As such, $\textrm{d}a_\textrm{l} / \textrm{d}m_\textrm{t}$ can itself be decomposed into a sum of construction costs per unit leaf area:
\begin{equation}\label{eq:daldmt}
\frac{\textrm{d}a_\textrm{l}}{\textrm{d}m_\textrm{t}}
= \big(\phi
 + \frac{\textrm{d}m_\textrm{s}}{\textrm{d}a_\textrm{l}} + \frac{\textrm{d}m_\textrm{b}}{\textrm{d}a_\textrm{l}} + \frac{\textrm{d}m_\textrm{r}}{\textrm{d}a_\textrm{l}}\big)^{-1}.
\end{equation}

The third term in eq \ref{eq:dhdt}, $\textrm{d}m_\textrm{t} / \textrm{d}P$, gives the fraction of net production ($\textrm{d}P / \textrm{d}t$) that is allocated to growth rather than reproduction or storage, while the final corresponds to net production given by eq. \ref{eq:dPdt}.

In a similar way, basal area increment can be expressed as the sum of increments in sapwood, bark \& heartwood area,
\begin{equation}\label{eq:dast}
\frac{\textrm{d}a_\textrm{st}}{\textrm{d}t}=
\underbrace{\strut \left(\frac{\textrm{d}a_\textrm{ss}}{\textrm{d}a_\textrm{l}} + \frac{\textrm{d}a_\textrm{sb}}{\textrm{d}a_\textrm{l}}\right) \times
\frac{\textrm{d}a_\textrm{l}}{\textrm{d}m_\textrm{t}} \times \frac{\textrm{d}m_\textrm{t}}{\textrm{d}P} \times \frac{\textrm{d}P}{\textrm{d}t}}_\textrm{expanding sapwood area}
+ \underbrace{\strut k_\textrm{s} a_\textrm{ss}}_\textrm{heartwood formation},
\end{equation}

while diameter growth is given by the geometric relationship between stem diameter ($D$) and $a_\textrm{st}$:
\begin{equation} \label{eq:dDdt}
\frac{\textrm{d}D}{\textrm{d}t}= \sqrt{\frac{\pi}{a_\textrm{st}}} \frac{\textrm{d}a_\textrm{st}}{\textrm{d}t}.
\end{equation}

While eqs. \ref{eq:dhdt}-\ref{eq:dDdt} are mathematical truisms and therefore hold for any model of plant growth, we require two additional elements before we can make explicit predictions on the effects of $\phi$ on growth requires :

i) A specific model describing how the various mass and area terms ($m_\textrm{l}, m_\textrm{s}, m_\textrm{b}, m_\textrm{r}, a_\textrm{l}, a_\textrm{s}, a_\textrm{b}$) vary relative to one another; and
ii) For the effects of varying $\phi$ on plant function to be specified.

To address the first of these challenges, we use a functional balance model[@yokozawa_foliage_1995; @falster_influence_2011] assuming 1) an allometric relation between total leaf area and plant height, and 2) a constant ratio between total leaf area and each of sapwood cross-sectional area, bark cross-sectional area, and total root mass. These assumptions, which are well supported by data, lead immediately to the set of equations for plant size described in Table \ref{tab:allometry} (see Supplementary material for derivations). Substituting from Table \ref{tab:allometry} into eqs. \ref{eq:dhdt},\ref{eq:dast},\ref{eq:dDdt} then gives an explicit model for plant growth in relation to size.

This model seems to capture well the intrinsically size-dependent nature of plant growth. Specifically, the model cpatures the following empirical patterns

- Declining leaf mass fraction with size
- Hump-shaped pattern of height growth rate with size resulting from changes size-dependent changes in 4 components of eq. \ref{eq:dhdt} (Fig. \ref{f-hump}).
- dbh and basal area growth?
- Decrease in RGR with size
- Large effect of reproductive allocation on growth rate in large trees (Fig. \ref{f-hmax}).

(?Figure: what drives decline at larger sizes --> Amount due to respiration, turnover, architecture, support costs, reproduction)

Having captured the intrinsic size-dependent nature of growth, we are ready to incorporate the effects of the effect of $\phi$ on growth. The direct effect of increasing $\phi$ is to increase the marginal cost of building leaf area (eq. \ref{eq:daldmt}) and thus reduce growth rate. However, to fully account for the influence of $\phi$ on growth we must also account for benefits of superior leaf construction: increased leaf longevity[@wright_world-wide_2004]. This is achieved by linking the turnover rate of leaf in eq. \ref{eq:dPdt} to $\phi$. We use an allometric scaling relationship of the form $k_\textrm{l}=\alpha_4 \, \phi^{\beta_4}$, which has been shown to describe patterns across diverse ecosystems[@wright_world-wide_2004] (Fig. \ref{fS-leaf}).

Unlike eq. \ref{eq:RGR}, eqs. \ref{eq:dhdt}, \ref{eq:dast} and \ref{eq:dDdt} predict a relationship between $\phi$ and growth that changes with plant size (Fig. \ref{f-LMA_growth_size}). Decreasing $\phi$ has two impacts on growth rate. First, lower $\phi$ increases leaf deployment rate ($\textrm{d}a_\textrm{l} / \textrm{d}m_\textrm{t}$ by economising on construction costs. Second, lower $\phi$ decreases net production ($\textrm{d}P / \textrm{d}t$), due to increased leaf turnover. Whether lower $\phi$ increases growth thus depends on the relative size of these two effects. When plants are small the effect on leaf deployment rate is bigger and so decreasing $\phi$ increases growth rate. When plants are large, the influence of $\phi$ on leaf deployment rate is diminished, because the costs of building other supportive tissues (other terms in eq. \ref{eq:daldmt}) are larger (Fig. XX). The net result is that at larger sizes, low $\phi$ is no longer advantageous for growth (Fig. \ref{f-LMA_growth_size}d).

- Correlation of growth rate with $\phi$ changes with size, Figure \ref{f-LMA_growth_size}
- Figure XXX: differential effects of $\phi$ on dP/dt and dA/dMt.

A number of other predictions arise directly from this model (Table \ref{tab:predictions}), all of which are supported with data from the BCI forest (?? we hope??).

Firstly, the responsiveness of growth rate to light changes with $\phi$ (Fig. \ref{f-LMA_growth_light}), such that species with low $\phi$ exhibit a wider range of growth rates, and also that the relationship between $\phi$ and growth rate flattens out under more stressed conditions.

Second, species with low $\phi$ are predicted to be less shade tolerant (Fig. \ref{f-LMA_wplcp}). At low $\phi$, leaf turnover is higher and thus a greater light income is needed to offset these costs. As previously suggested by @givnish_adaptation_1988, shade-tolerance also decreases with height because as size increase, the total amount of energy needed to offset respiratory and turnover costs in the stem also increases.

These first two predictions would also be expected to hold under any environmental conditions decreasing photosynthetic income (e.g. low temperature of nutrients), not just decreased light.

Finally, we predict that all species will show a strong onto-genetic increase in $\phi$ with size (Fig. \ref{f-LMA_optim}). To arrive at this prediction we solved for the value of $\phi$ maximising growth rate at a given light environment. At larger sizes, the benefits of cheap leaf construction diminish, while the costs (increased leaf turnover rate) remain. This means that at larger sizes growth rate is maximised with lower values of $\phi$ (Fig. \label{f-LMA_growth_size}).

\newpage

# Tables
\begin{table}[h]
\caption{Predictions and evidence}

\centering

\begin{tabular}{p{7cm}p{4cm}p{4cm}}
\hline
Prediction & For & Against\\ \hline

1. Growth rates show a hump-shaped relationship with size& &\\ \\
2. The influence of traits on growth rate changes with size& &\\ \\
2a. Low LCC-LL species have faster growth rates as seedlings& &\\ \\
2b. There is no consistent relationship between LCC-LL and growth rates among larger plants& &\\ \\
3. The ranking of growth rates across species does not change with light environment& &\\ \\
4. The growth rate of low LCC species is more sensitive to changes in light& &\\ \\
5. Low LCC species are less shade tolerant& &\\ \\
6. Shade-tolerance decreases with size& &\\ \\
7. Faster decline in shade tolerance with size for low LCC species& &\\ \\
8. For all species, LCC increases with size& &\\ \hline
\end{tabular}
\label{tab:predictions}
\end{table}

\newpage

\begin{table}[h]
\caption{Equations for an allometric growth model}
\centering

\begin{tabular}{lll}
\hline
Variable & Function & Derivative\\ \hline
leaf area & $a_\textrm{l}=\alpha_1 \, h^{\beta_1}$ & $\frac{\textrm{d}h}{\textrm{d}a_\textrm{l}}= -\beta_1\big(\frac{a_\textrm{l}}{\alpha_1}\big)^{-(\beta_1+1)}$\\
leaf mass & $m_\textrm{l}=\phi \, a_\textrm{l} $ & $\frac{\textrm{d}m_\textrm{l}}{\textrm{d}a_\textrm{l}}=\phi$\\
sapwoood area & $a_\textrm{ss}=\theta^{-1} \, a_\textrm{l}$ & $\frac{\textrm{d}a_\textrm{ss}}{\textrm{d}t} =\theta^{-1} \, \frac{\textrm{d}a_\textrm{l}}{\textrm{d}t}$\\
sapwood mass&$m_\textrm{ss}=\theta^{-1} \, \rho \, \eta_c \, a_\textrm{l} \, h $ & $\frac{\textrm{d}m_\textrm{ss}}{\textrm{d}a_\textrm{l}}=\theta^{-1}\, \rho\, \eta_c\, \big( h + a_\textrm{l}\, \frac{\textrm{d}h}{\textrm{d}a_\textrm{l}} \big)$\\
bark area & $a_\textrm{sb}=b \, \theta^{-1} \, da_\textrm{l}$ & $\frac{\textrm{d}a_\textrm{sb}}{\textrm{d}t}=b \, \theta^{-1} \, \frac{\textrm{d}a_\textrm{l}}{\textrm{d}t}$\\
bark mass&$m_\textrm{b}=b\, \theta^{-1} \, \rho \, \eta_c \, a_\textrm{l} \, h $ & $\frac{\textrm{d}m_\textrm{b}}{\textrm{d}a_\textrm{l}}=b \, \theta^{-1} \, \rho \, \eta_c\big( h + a_\textrm{l} \, \frac{\textrm{d}h}{\textrm{d}A} \big)$\\
heartwood area & $a_\textrm{sh}=\int_0^t \frac{\textrm{d}a_\textrm{sh}}{\textrm{d}t}(t^\prime) \, dt^\prime$ & $\frac{\textrm{d}a_\textrm{sh}}{\textrm{d}t}=k_\textrm{s} \, a_\textrm{ss}$\\
heartwood mass & $m_\textrm{sh}=\int_0^t \frac{\textrm{d}m_\textrm{sh}}{\textrm{d}t}(t^\prime) \, dt^\prime$ & $\frac{\textrm{d}m_\textrm{sh}}{\textrm{d}t}=k_\textrm{s} \, m_\textrm{ss}$\\
root mass & $m_\textrm{r}=\alpha_3 \, a_\textrm{l}$ & $\frac{\textrm{d}m_\textrm{r}}{\textrm{d}a_\textrm{l}}= \alpha_3$ \\\hline
\end{tabular}
\label{tab:allometry}
\end{table}

\newpage

# Figures

![**The relationship between leaf-construction cost and growth rate varies with plant size.**
For XXX species growing in lowland forest at BCI Panama, we estimated the potential growth rate of individual's in that species at a given size and plotted this against leaf-construction cost, with size of circle indicating the number of data points used to estimate potential growth rate. \label{f-RGR}](../output/figs/GR-LCC.pdf)

![**The expected correlation between leaf-construction cost and growth rate changes with plant size.**
Predictions from the model on the relationship between leaf-construction cost and height growth rate under ideal conditions at a range of sizes.
\label{f-LMA_growth_size}](../output/figs/growth-height.pdf)

\newpage

![**Reproductive allocation has a strong influence on tree growth at larger sizes.**
We use maximum height as an indicator of reproductive allocation. Generally, plants with great maximum height tend to defer reproductive investment until a larger size is reached, which increase their growth rate relative to species with lower maximum height and higher allocation of resources to reproduction.
\label{f-hmax](../output/figs/SI-mass_fraction.pdf)
\newpage

![**Plants with cheaper leaf construction are more sensitive to changing light.**
Plot shows predicted relationship between height growth rate and leaf-construction cost under a range of shading environments for plants 0.25m tall.
\label{f-LMA_growth_light}](../output/figs/growth-light.pdf)

\newpage

![**Plants with cheaper leaf construction and greater height are less shade tolerant**
Shade tolerances is quantified as the maximum leaf area above the plant before growth rate reaches zero. Plot shows predicted relationship with leaf-construction cost for plants at a range of heights.
\label{f-LMA_wplcp}](../output/figs/LMA_wplcp.pdf)

\newpage

![**Leaf construction cost increases through ontogeny.**
Plot shows the value of $\phi$ maximising growth rate for plants at a given height and under a range of shading environments.
\label{f-LMA_optim}](../output/figs/LMA_optim.pdf)

\newpage

# Supplementary material

![**Leaf turnover decreases with leaf-construction cost.**
Data from @wright_world-wide_2004 for 678 species from 51 sites, each point giving a species-average. Lines show standardised major axis lines fitted to data from each site, with intensity of shading adjusted according to strength of the relationship.\label{fS-leaf}](../output/figs/SI-leaf.pdf)

![**Hump-shaped relationship between growth rate and size.**
\label{f-hump}](../output/figs/SI-size-dhdt.pdf)

![**Change in allocation with size.**
\label{f-mass_fraction}](../output/figs/SI-mass_fraction.pdf)




\newpage
\begin{table}[h]
\caption{Model parameters}
\centering

\include{table-pars}

\label{tab:params}
\end{table}

\newpage

## A simple allometric model of plant function

Here we describe an allometric model linking the various size dimensions of a plant required by most ecologically realistic vegetation models (i.e. =mass of leaves, mass of sapwood, mass of bark, mass of fine roots) to a plant height.

### Leaf area

Based on empirically observed allometries (see main text), we assume an allometric log-log scaling relationship between the accumulated leaf area of a plant and its height:

\begin{equation}\label{eq:ha}
a_\textrm{l}=\alpha_1 \, h^{\beta_1}.
\end{equation}

Note, scaling relationship reversed from @falster_influence_2011].

### Mass of sapwood

We follow the model of[@yokozawa_foliage_1995] describing the vertical distribution of leaf area within the crowns of individual plants. This model can account for a variety of canopy profiles through a single parameter $\eta$. Setting $\eta=1$ results in a conical canopy, as seen in many conifers, while higher values, e.g. $\eta=12$ , give a top-weighted canopy profile similar to those seen among angiosperms. Let $S(z,h)$ be the sapwood area at height $z$ for a plant with top height $h$. Following Yokozawa and Hara (1995) we assume a relationship between $S(z,h)$ and height such that

\begin{equation}\label{eq:crown1}
\frac{S(z,h)}{S(0,h)}= \big(1-\big(\frac{z}{h}\big)^\eta\big)^2.
\end{equation}

We also assume that each unit of sapwood area supports a fixed area of leaf (the pipe model, @shinozaki_quantitative_1964), so that the total canopy area of a plant relates to basal sapwood area $S(0,h)$:

\begin{equation}\label{eq:crown2}
\frac{m_\textrm{l}}{\phi}= \theta \, S(0,h).
\end{equation}

Integrating $S(z,h)$ gives a solution for the total mass of sapwood in the plant:

\begin{equation}\label{eq:ms1}
m_\textrm{s}=\rho \, \int_0^h \, S(z,h) \, \textrm{d}z= \rho \, S(0,h) \, h \, \eta_c, \end{equation}

where $\eta_c=1-\frac{2}{1+\eta} + \frac{1}{1+2\eta}$[@yokozawa_foliage_1995]. Substituting from eq. \ref{eq:crown2} into eq. \ref{eq:ms1} gives an expression for sapwood mass as a function leaf area and height:

\begin{equation}\label{eq:ms2}
m_\textrm{s}=\rho \, \eta_c \, \theta^{-1} \, a_\textrm{l} \, h.
\end{equation}

### Bark mass

Bark and phloem tissue are modelled using an analogue of the pipe model, leading to a similar equation as that for sapwood mass (eq. \ref{eq:ms2}). Cross sectional-area of bark per unit leaf area is assumed to be a constant fraction $b$ of sapwood area per unit leaf area such that

\begin{equation}\label{eq:mb}
m_\textrm{b}=b m_\textrm{s}.
\end{equation}

### Root mass

Also consistent with pipe-model assumption, we assume a fixed ratio of root mass per unit leaf area

\begin{equation}\label{eq:mr}
m_\textrm{r}=\alpha_3 \, a_\textrm{l}.
\end{equation}

Even though nitrogen and water uptake are not modelled explicitly, imposing a fixed ratio of root mass to leaf area ensures that approximate costs of root production are included in calculations of carbon budget.


# References
