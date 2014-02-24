% Growth trajectories: a new way of understanding the influence of the leaf economics spectrum on plant growth
% Daniel Falster; Rich FitzJohn

# Abstract

* Background/Question/Methods: Plant species differ in many structural and allocation traits (SATs), each with its own distinct influence on plant function. Here, we show how these SATs combine with size and light environment to determine growth and survival throughout a plant’s life. We introduce a new theoretical framework, based on carbon fluxes and allocation, which extends earlier models designed to understand differences in the relative growth rate of seedlings.
* In both old and new models, growth rate is decomposed into several components, each influenced by traits. Unlike models that focus exclusively on seedlings, the new framework explains how the influence of traits is moderated by plant size. This leads to specific hypotheses about the effect of some well-known SATs, including leaf mass per area, wood density, leaf nitrogen content and seed size, on plant growth.
* Results: The new model suggests four fundamental predictions about growth. First, growth rates show a hump-shaped relationship with plant size. Second, the expected correlation between growth rate and individual SATs varies with size, with some relationships increasing in strength, others decreasing, and some reversing direction as size increases. Third, SATs moderate how growth rate responds to resource levels. Fourth, shade tolerance decreases with size and select SATs.
* Conclusions: Combined, these results suggest that individual SATs do not necessarily imply fast or slow growth, but rather define a trajectory of growth with size and light level that determines a species' growth strategy.

# Introduction

Ecologists have identified a variety of traits influencing plant function; the challenge now is to understand how these traits influence growth, mortality and fecundity across the life cycle.

Apparent change in relationship between LMA and RGR (Figure \ref{f-RGR}).

# Model

## Mass production

## Net photosynthetic Income

Equations for the net mass growth of the plant can be derived based on well established physiology:

\begin{equation}\label{eq:dPdt}
\frac{dP}{\textrm{d}t}= \underbrace{\strut c Y}_\textrm{yield} (\underbrace{\strut m_l \, \phi^{-1} \, A(E)}_\textrm{photosynthesis} - (\underbrace{\strut \, m_l \, \phi  \, r_\textrm{l} + m_s \, r_\textrm{s} + m_b \, r_\textrm{b} + m_r \, r_\textrm{r}}_\textrm{respiration})) - (\underbrace{\strut m_l \, k_\textrm{l} + m_s \, k_\textrm{s} + m_b \, k_\textrm{b} + m_r \, k_\textrm{r}}_\textrm{turnover}).
\end{equation}

where $A(E)$ is photosynthetic rate per unit leaf area in a given light environment $E$,, $r_l, r_s, r_b r_r$ are respiration rates per unit leaf, sapwood, bark and root mass, and $k_l, k_s, k_b, k_r$ are turnover rates of leaf, sapwood, bark and root mass.

## The classic RGR decomposition

The classic decomposition of RGR can be derived as follows. First, note that RGR is given by

\begin{equation}\label{eq:RGR1}
\textrm{RGR}=\frac{1}{m_t} \frac{\textrm{d}P}{\textrm{d}t}.
\end{equation}

Now substitute, eq. \ref{eq:dPdt} into eq. \ref{eq:RGR} and ignore all turnover and all respiration terms except leaf. This gives

\begin{equation}\label{eq:RGR2}
\textrm{RGR}=\left(c Y (A(E) - r_\textrm{l})\right) \times \phi^{-1} \times \frac{m_l}{m_t}. \end{equation}

Missing from eq. \ref{eq:RGR2} is

- turnover
- respiration of non-leaf tissues
- costs of deploying new tissues.

## Decomposition of height growth rate

Height growth rate, like RGR above, can also be decomposed into the product of other terms:

\begin{equation} \label{eq:dhdt}
\frac{\textrm{d}h}{\textrm{d}t}=\frac{\textrm{d}h}{\textrm{d}a_l} \times \frac{\textrm{d}a_l}{\textrm{d}m_t} \times \frac{\textrm{d}m_t}{\textrm{d}P} \times \frac{\textrm{d}P}{\textrm{d}t}.
\end{equation}

The components of this model are as follows:

- $h$ is the height of the plant,
- $a_l$ is total leaf area
- $\frac{\textrm{d}h}{\textrm{d}a_l}$ is the growth in height per unit growth in leaf area; accounting for the architectural strategy of the plant. (Some species tend to leaf out more than grow tall, while other species emphasise vertical extension.)
- $\frac{\textrm{d}a_l}{\textrm{d}m_t}$ is the rate of leaf area growth per unit mass growth for the plant, accounting for costs of deploying a unit of leaf area, including the cost of building the leaf itself and the various support structures required.
- $\frac{\textrm{d}m_t}{\textrm{d}P}$ is the fraction of mass produced by the plant allocated to reproduction, and
- $\frac{\textrm{d}P}{\textrm{d}t}$ is the mass production rate of the plant.

Thus height growth rate is given by the product of terms representing architectural layout, tissue costs, allocation to growth, mass production.

Each of these terms can be further decomposed, allowing the effect of traits to be clarified. The term $\frac{\textrm{d}P}{\textrm{d}t}$ is given in eq. \ref{eq:dPdt}. In addition, the term  $\frac{\textrm{d}a_l}{\textrm{d}m_t}$ can be expressed as the inverse of a sum of construction costs per unit leaf area:

\begin{equation}\label{eq:daldmt}
\frac{\textrm{d}a_l}{\textrm{d}m_t}= \frac{1}{\frac{\textrm{d}m_t}{\textrm{d}a_l}}
=  \frac{1}{\frac{\textrm{d}m_l}{\textrm{d}a_l}
 + \frac{\textrm{d}m_s}{\textrm{d}a_l} + \frac{\textrm{d}m_b}{\textrm{d}a_l} + \frac{\textrm{d}m_r}{\textrm{d}a_l}}.
\end{equation}

<!-- Thus
\begin{equation}\label{eq:dhdt2}
\frac{\textrm{d}h}{\textrm{d}t}=\frac{\textrm{d}h}{\textrm{d}a_l}  \frac{1}{\frac{\textrm{d}m_l}{\textrm{d}a_l}
 + \frac{\textrm{d}m_s}{\textrm{d}a_l} + \frac{\textrm{d}m_b}{\textrm{d}a_l} + \frac{\textrm{d}m_r}{\textrm{d}a_l}} \frac{\textrm{d}m_t}{\textrm{d}P}  c Y(m_l \, \phi^{-1} \, A(E) - (m_l \, \phi  \, r_\textrm{l} + m_s \, r_\textrm{s} + m_b \, r_\textrm{b} + m_r \, r_\textrm{r})) - (m_l \, k_\textrm{l} + m_s \, k_\textrm{s} + m_b \, k_\textrm{b} + m_r \, k_\textrm{r})
.\end{equation}
 -->

## Decomposition of diameter growth rate

Note that diameter is related to the total stem cross section $a_\textrm{st}$ as $D = 2 \sqrt{\frac{a_\textrm{st}}{\pi}}$, such that diameter increment is related to area increment:
\begin{equation} \label{eq:dbh1}
\frac{\textrm{d}D}{\textrm{d}t} = \frac{2}{\pi D} \frac{\textrm{d}a_\textrm{st}}{\textrm{d}t}.
\end{equation}

Basal area increment in turn is the sum of increments in sapwood, bark \& heartwood area:
\begin{equation} \label{eq:dast}
\frac{\textrm{d}a_\textrm{st}}{\textrm{d}t}= \frac{\textrm{d}a_\textrm{ss}}{\textrm{d}t} + \frac{\textrm{d}a_\textrm{sb}}{\textrm{d}t} + \frac{\textrm{d}a_\textrm{sh}}{\textrm{d}t}.
\end{equation}

Thus,

\begin{equation} \label{eq:dbh2}
\frac{\textrm{d}D}{\textrm{d}t} = \frac{2}{\pi D} \left(\frac{\textrm{d}a_\textrm{ss}}{\textrm{d}t} + \frac{\textrm{d}a_\textrm{sb}}{\textrm{d}t} + \frac{\textrm{d}a_\textrm{sh}}{\textrm{d}t}\right).
\end{equation}

## A simple allometric model of plant function

Equations \ref{eq:dhdt},\ref{eq:daldmt},\ref{eq:dbh1} are mathematically true and thus hold for any model of plant growth. To make explicit predictions about the influence of traits on growth requires three additional features

1. A specific model describing how the various mass terms ($m_l, m_s, m_b, mr$) vary with plant height.
2. Estimates of various parameters, respiration rates and turnover rates and how these vary with size or light environment.
3. Specification of how leaf traits enter into the eq. \ref{dhdt}.

For these we adopt a trait and size based model developed by [@falster_influence_2011], drawing heavily on [@yokozawa_foliage_1995]. (similar model by makela??). This model is based on four simple assumptions:

1. An allometric relation between total leaf area and plant height
2. The pipe model, relating cross-sectional area of sapwood and bark to total leaf area.
3. A fixed ratio of root mass per leaf area.
4. The relative distribution of leaf area within the plant's crown is  constant through ontogeny.
5. Sapwood area has given lifespan.

Items 1-3 are (??) supported by data --> plots from allometry database. Less data available to assess 4-5, but available evidence suggests these are reasonable first approximations.

Based on these assumptions

- sapwood area increment is directly proportional to leaf area increment
- heartwood are increment is proportional to sapwood turnover rate.

These assumptions lead immediately to the set of equations for plant size described in table Table \ref{tab:allometry} (see Supplementary material for derivations).

Substituting the features from table \ref{tab:allometry} into eq. \ref{eq:dbh2} leads to the following equation for dbh growth:

\begin{equation}\label{eq:dbh3}
\frac{\textrm{d}D}{\textrm{d}t} = \frac{2}{\pi D \theta } \left((1+b) \frac{\textrm{d}a_l}{\textrm{d}t} + k_s a_l\right).\end{equation}

This equation shows that diameter growth rate is related to height growth rate (as both are dependent on $\frac{\textrm{d}a_l}{\textrm{d}t}$), but with an allometric adjustment and the added element of heartwood formation.

## Influence of traits

Leaf mass per area captures a trade-off between leaf construction cost and rate of leaf turnover, reflecting the well-known correlation between measures of construction cost (LMA, leaf tissue density, or leaf thickness) and leaf lifespan [@wright_world-wide_2004]. Thus the turnover rate of leaf, $k_l$, is linked to LMA (Fig. \ref{fS-leaf}):
\begin{equation}\label{eq:LMA-KL}
k_l=\alpha_4 \, \phi^{\beta_4}.
\end{equation}

## Other parameters

Remaining parameters listed in Table \ref{tab:params}.

# Results

Predictions summarised \ref{tab:predictions}.

## Growth rate shows hump-shaped pattern with with size

Figure \ref{f-hump}: Hump-shaped curve, 4 components

Figure: what drives decline at larger sizes --> Amount due to respiration, turnover, architecture, support costs, reproduction

## Correlation of growth rate with LMA changes with size

Figure \ref{f-LMA_growth_size}

- also include gif in suppmat?

Figure \ref{f-mass_fraction}: differential effects of LMA on dP/dt and dA/dMt. Allocation.

## Responsiveness of growth to light changes with LMA

Figure \ref{f-LMA_growth_light}: sensitivity of growth to light

## Low LMA species less shade tolerant

Figure \ref{f-LMA_wplcp}: LMA vs wplcp for different size classes

Shade-tolerance also decreases with size. (As previously recognised by Givnish)

- see previous figure \ref{f-LMA_wplcp}:
- Faster decline in shade tolerance with size for low LMA species ??

More than just shade - any prolonged period of low productivity.

- low LMA on less productive soils

## Ontogenetic shift towards higher LMA with size

Figure \ref{f-LMA_optim}: LMA maximising growth rate against size for different light environments


# Discussion


1. Growth rate is size-dependent
2. Strength of trait-correlation varies with size. Could be increasing, decreasing, reversing
3. Species not 'fast' or 'slow', rather have a trajectory
4. We need theory to make sense of data

\newpage

# Tables

\begin{table}[h]
\caption{Equations for an allometric growth model}
\centering

\begin{tabular}{lll}
\hline
Variable & Function  & Derivative\\ \hline
leaf area & $a_l=\alpha_1 \, h^{\beta_1}$ & $\frac{\textrm{d}h}{\textrm{d}a_l}= -\beta_1\left(\frac{a_l}{\alpha_1}\right)^{-(\beta_1+1)}$\\
leaf mass & $m_l=\phi \, a_l $ & $\frac{\textrm{d}m_l}{\textrm{d}a_l}=\phi$\\
sapwood mass&$m_s=\theta^{-1} \, \rho \, \eta_c \, a_l \, h $ & $\frac{\textrm{d}m_s}{\textrm{d}a_l}=\theta^{-1}\, \rho\, \eta_c\, \left( h + a_l\, \frac{\textrm{d}h}{\textrm{d}a_l} \right)$\\
bark mass&$m_b=b\, \theta^{-1} \, \rho \, \eta_c \, a_l \, h $ & $\frac{\textrm{d}m_b}{\textrm{d}a_l}=b \, \theta^{-1} \, \rho \, \eta_c\left( h + a_l \, \frac{\textrm{d}h}{\textrm{d}A} \right)$\\
root mass & $m_r=\alpha_3 \, a_l$ & $\frac{\textrm{d}m_r}{\textrm{d}a_l}= \alpha_3$ \\
sapwoood area & $a_\textrm{ss}=\theta^{-1} \, a_l$ & $\frac{\textrm{d}a_\textrm{ss}}{\textrm{d}t} =\theta^{-1} \, \frac{\textrm{d}a_l}{\textrm{d}t}$\\
bark area & $a_\textrm{sb}=b \, \theta^{-1} \, da_l$ & $\frac{\textrm{d}a_\textrm{sb}}{\textrm{d}t}=b \, \theta^{-1} \, \frac{\textrm{d}a_l}{\textrm{d}t}$\\
heartwood area & $a_\textrm{sh}=\int_0^t \frac{\textrm{d}a_\textrm{sh}}{\textrm{d}t}(t^\prime) \, dt^\prime$ & $\frac{\textrm{d}a_\textrm{sh}}{\textrm{d}t}=k_s\theta^{-1} a_l$\\\hline
\end{tabular}
\label{tab:allometry}
\end{table}

\newpage
\begin{table}[h]
\caption{Model parameters}
\centering

\include{table-pars}

\label{tab:params}
\end{table}

\newpage

\begin{table}[h]
\caption{Predictions and evidence}

\centering

\begin{tabular}{p{7cm}p{4cm}p{4cm}}
\hline
Prediction & For & Against\\ \hline

1. Growth rates show a hump-shaped relationship with size& &\\ \\
2. The influence of traits on growth rate changes with size& &\\ \\
2a. Low LMA-LL species have faster growth rates as seedlings& &\\ \\
2b. There is no consistent relationship between LMA-LL and growth rates among larger plants& &\\ \\
3. The ranking of growth rates across species does not change with light environment& &\\ \\
4. The growth rate of low LMA species is more sensitive to changes in light& &\\ \\
5. Low LMA species are less shade tolerant& &\\ \\
6. Shade-tolerance decreases with size& &\\ \\
7. Faster decline in shade tolerance with size for low LMA species& &\\ \\
8. For all species, LMA increases with size& &\\ \hline
\end{tabular}
\label{tab:predictions}
\end{table}

\newpage

# Figures

![**Relationship between leaf mass per area and relative growth rate varies with plant size.**
\label{f-RGR}](../figs/RGR.pdf)

![**Hump-shaped relationship between growth rate and size.**
\label{f-hump}](../figs/hump.pdf)

![**Correlation between LMA and growth changes with plant size.**
\label{f-LMA_growth_size}](../figs/LMA_growth_size.pdf)

![**Change in allocation with size.**
\label{f-mass_fraction}](../figs/mass_fraction.pdf)

![**Sensitivity of growth to light.**
\label{f-LMA_growth_light}](../figs/LMA_growth_light.pdf)

![**Whole plant light compensation point.**
\label{f-LMA_wplcp}](../figs/LMA_wplcp.pdf)

![**LMA maximising growth rate against size for different light environments.**
\label{f-LMA_optim}](../figs/LMA_optim.pdf)

\newpage

# Supplementary figures

![**Trade-off between leaf mass per area and leaf turnover rate.**
Across species, leaf mass per area is inversely related to leaf turnover rate. Data from Wright et al 2004[@wright_world-wide_2004] for 678 species from 51 sites. Green lines show standardised major axis lines fitted to data from each site, with intensity of shading adjusted according to strength of the relationship.\label{fS-leaf}](../figs/SI-leaf.pdf)

\newpage

# Supplementary material

## A simple allometric model of plant structure

Here we describe an allometric model linking the various size dimensions of a plant required by most ecologically realistic vegetation models (i.e.  =mass of leaves, mass of sapwood, mass of bark, mass of fine roots) to a  plant height.

### Leaf area

Based on empirically observed allometries (see main text), we assume an allometric log-log scaling relationship between the accumulated leaf area of a plant and its height:

\begin{equation}\label{eq:ha}
a_l=\alpha_1 \, h^{\beta_1}.
\end{equation}

**note, scaling relationship reveresed from Falster et al 20011***

### Mass of sapwood

We follow the model of [@yokozawa_foliage_1995] describing the vertical distribution of leaf area within the crowns of individual plants. This model can account for a variety of canopy profiles through a single parameter  $\eta$. Setting $\eta=1$ results in a conical canopy, as seen in many conifers, while higher values, e.g. $\eta=12$ , give a top-weighted canopy profile similar to those seen among angiosperms. Let $S(z,h)$ be the sapwood area at height  $z$ for a plant with top height $h$. Following Yokozawa and Hara (1995) we assume a relationship between  $S(z,h)$ and height such that

\begin{equation}\label{eq:crown1}
\frac{S(z,h)}{S(0,h)}= \left(1-\left(\frac{z}{h}\right)^\eta\right)^2.
\end{equation}

We also assume that each unit of sapwood area supports a fixed area of leaf (the pipe model, [@shinozaki_quantitative_1964]), so that the total canopy area of a plant relates to basal sapwood area $S(0,h)$:

\begin{equation}\label{eq:crown2}
\frac{m_l}{\phi}= \theta \, S(0,h).
\end{equation}

Integrating $S(z,h)$ gives a solution for the total mass of sapwood in the plant:

\begin{equation}\label{eq:ms1}
m_s=\rho \, \int_0^h \, S(z,h) \, \textrm{d}z= \rho \, S(0,h) \, h \, \eta_c,
\end{equation}

where $\eta_c=1-\frac{2}{1+\eta} + \frac{1}{1+2\eta} $ [@yokozawa_foliage_1995]. Substituting from eq. \ref{eq:crown2} into  eq. \ref{eq:ms1} gives an expression for sapwood mass as a function leaf area and height:

\begin{equation}\label{eq:ms2}
m_s=\rho \, \eta_c \, \theta^{-1} \, a_\textrm{l} \, h.
\end{equation}

<!--
### Vertical distribution of leaf area

Now let $q(z,h)$ the probability density of leaf area at height $z$  and $Q(z,h)$ be the fraction of a plant’s leaf above height $z$. The pipe model is assumed to hold within individual plants, as well as across plants of different size. from eq. \ref{eq:crown2} it follows that

\begin{equation}
Q(z,h)=\int_z^h q(z^\prime,h) \; \textrm{d}z^\prime= \left(1-\left(\frac{z}{h}\right)^\eta\right)^2.
\end{equation}

Differentiating with respect to $z$ then yields a solution for $q(z)$, the probability density of leaf area as a function of height:

\begin{equation}
q=2\eta(1-z^\eta h^{-\eta}) z^{\eta-1} h^{-\eta}.
\end{equation}
-->

### Bark mass

Bark and phloem tissue are modelled using an analogue of the pipe model, leading to a similar equation as that for sapwood mass (eq. \ref{eq:ms2}). Cross sectional-area of bark per unit leaf area is assumed to be a constant fraction $b$ of sapwood area per unit leaf area such that

\begin{equation}\label{eq:mb}
m_\textrm{b}=b m_s.
\end{equation}

### Root mass

Also consistent with pipe-model assumption, we assume a fixed ratio of root mass per unit leaf area

\begin{equation}\label{eq:mr}
m_\textrm{r}=\alpha_3 \, a_l.
\end{equation}

Even though nitrogen and water uptake are not modelled explicitly, imposing a fixed ratio of root mass to leaf area ensures that approximate costs of root production are included in calculations of carbon budget.

# References
