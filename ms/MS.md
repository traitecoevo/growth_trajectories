% Growth trajectories: untangling the link between leaf traits and plant growth
% Daniel Falster; Rich FitzJohn;
<!-- Karou Kitajima; Joe Wright -->

# Abstract

* The well documented trade-off between leaf construction cost and leaf turnover rate, known as the leaf economics spectrum, captures a core difference in the strategy plants use to generate and invest surplus energy.
* In seedlings, leaf construction cost correlates tightly with plant growth; the strength of the relationship leading many to suggest leaf construction cost as a generic indicator of potential growth rate. Although existing models suggests this relationship should extend to larger plants, numerous attempts to link leaf construction cost with growth rate in trees have been unsuccessful.
* Here we use a mechanistic model based on carbon balance and allocation to show, firstly, why the link between leaf construction cost and growth rate breaks down in larger plants. We find that the influence of leaf construction cost on growth changes with size because the benefits of cheap leaf construction diminish at larger sizes, as an increasing fraction of the plant is comprised of woody tissues.
* Secondly, we use our model to generate ??4?? other fundamental predictions about plant growth and the influence leaf construction cost; these predictions are verified using data from experimental plantings and long-term plot surveys of the tropical forest at Barro Colorado Island, Panama.
* Combined, out model ...
* Thus in contrast to earlier models and theory, we suggest that a given leaf construction cost value does not necessarily imply fast or slow growth, but rather defines a trajectory of growth with size and light environment, and that this trajectory is what defines a species' growth strategy.

# Main text

Plant growth is the foundation of the terrestrial biosphere, powering foodwebs and much of human activity[@raupach_carbon_2010]. While most higher plants have the same basic resource requirements (light water, nutrients) and physiological bauplan, key quantitative differences exist in the quality of construction and allocation to key plant components (leaves, stems, roots)[@westoby_plant_2002;@niklas_evolution_2000]. Leaf construction cost ($\phi$) -- the amount of mass used to construct a unit of leaf area -- is one dimension of variation, varying approx 40 fold among species[@wright_world-wide_2004]. Low $\phi$ can, in principle, accelerate growth, as leaves which are cheap to build enable faster canopy expansion[@blackman_compound_1919; @westoby_time_2000]. Indeed, growth rate in seedlings is strongly and negatively correlated with $\phi$ (Fig. \ref{f-RGR}a)[@cornelissen_seedling_1996; @wright_cross-species_2000]. It has been suggested that low $\phi$ might also give a growth advantage in larger plants[@enquist_general_2007], with $\phi$ often being directly equated with growth rate (??). However, an increasing number of empirical studies have failed to detect any such effect in large trees (Fig. \ref{f-RGR}c)[@wright_functional_2010, @herault_functional_2011, @poorter_are_2008]. The growing discord between theoretical expectation and empirical result indicates a strong need for a new theoretical framework through which the influence of $\phi$ on plant growth can be reassessed.

Here we extend the theoretical model used to link $\phi$ with growth rate in seedlings to explicitly include influences of size, light environment, and effects of leaf turnover, and thereby generate XX predictions about plant demography and links to $\phi$ (Table \ref{tab:predictions}). The new model captures the observed change in correlation between $\phi$ and growth rate with increasing size (Fig. \ref{f-RGR}), and also between $\phi$ and shade tolerance (Fig. \ref{f-LMA_wplcp}). Moreover, we provide a novel explanation for the observed tendency of $\phi$ to increase through ontogeny (Fig. \ref{f-LMA_optim}). Together, these findings help to integrate a large number of empirical and theoretical findings about plant growth and its link to traits.

We begin with a standard model for the production of live biomass, $dP / \textrm{d}t$, given by the difference income (total photosynthesis) and losses (respiration and turnover) for the different plant components:

\begin{equation}\label{eq:dPdt}
\frac{dP}{\textrm{d}t}= \underbrace{\strut Y}_\textrm{yield} \big(\underbrace{\strut \frac{m_\textrm{l}}{\phi} \, A}_\textrm{photosynthesis} - \big(\underbrace{\strut \, \frac{m_\textrm{l}}{\phi} \, r_\textrm{l} + \sum_\textrm{i=b,s,r}{m_\textrm{i} \, r_\textrm{i}}}_\textrm{respiration}\big)\big) - \big(\underbrace{\strut \sum_\textrm{i=l,b,s,r}{m_\textrm{i} \, k_\textrm{i}}}_\textrm{turnover}\big).
\end{equation}
Here, $m,r$, and $k$ refer to the mass, respiration rate, and turnover rate of different tissues with subscripts $l$=leaves, $b$=bark, $s$=sapwood and $r$=roots, while $A$ is the assimilation rate per leaf area and $Y$ is yield, which accounts for growth respiration (see Table \ref{tab:params} for units and definitions). Photosynthesis is proportional to leaf area, $a_\textrm{l} = m_\textrm{l} / \phi$, while total mass of living tissues $m_\textrm{t}=m_\textrm{l}+m_\textrm{b}+m_\textrm{s}+m_\textrm{r}.$

Current theory emphasises the relationship between $\phi$ and relative growth in mass for seedlings, $R_{m_\textrm{t}}= \textrm{d}P / \textrm{d}t \, / \, m_\textrm{t}$. For seedlings, which are young and mostly leaf, it is reasonable to ignore all turnover terms as well as the respiration terms for non-leaf tissues. Net production then becomes a linear function of leaf area and net photosynthesis per leaf area ($A_\textrm{net} = Y(A - r_\textrm{l})$), making $R_{m_\textrm{t}}$ a linear function of $\phi$:
\begin{equation}\label{eq:RGR}
R_{m_\textrm{t}} \approx A_\textrm{net} \times \phi^{-1} \times \frac{m_\textrm{l}}{m_\textrm{t}}. \end{equation}
Although eq. \ref{eq:RGR} adequately describes growth in seedlings
[@wright_cross-species_2000], the predicted link between $\phi$ and growth is absent in larger plants (Fig. \ref{f-RGR}). Moreover, a mdoel formulated via $R_{m_\textrm{t}}$ is difficult to link with the types of data routinely collected for large trees, namely plant height ($h$), stem basal area ($a_\textrm{st}$), and stem diameter $D$.

To properly model growth in any of these quantities, it is necessary to account not just for mass production, but also for the costs of building new tissues, allocation to reproduction, and architectural layout. For height growth, this can achieved by decomposing $\textrm{d}h / \textrm{d}t$ into a product of physiologically relevant terms:

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

The third term in eq \ref{eq:dhdt}, $\textrm{d}m_\textrm{t} / \textrm{d}P$, gives the fraction of net production ($\textrm{d}P / \textrm{d}t$) that is allocated to growth rather than reproduction or storage.

Eqs. \ref{eq:dhdt} and \ref{eq:daldmt} are mathematically true and must therefore hold for any model of plant growth. In other words, we are yet to make any assumptions about the nature of plant construction. However, to make explicit predictions about requires first, a specific model describing how the various mass and area terms ($m_\textrm{l}, m_\textrm{s}, m_\textrm{b}, m\textrm{r}, a_\textrm{l}, a_\textrm{s}, a_\textrm{b}$) vary relative to one another; and second, for the effects of varying $\phi$ on plant function to be specified. To address the first of these challenges, we use a simple model[@yokozawa_foliage_1995; @falster_influence_2011] assuming:

1. An allometric relation between total leaf area and plant height
2. A constant ratio between total leaf area and each of sapwood cross-sectional area, bark cross-sectional area, and total root mass.

These assumptions, which are well supported by data (Fig. XX), lead immediately to the set of equations for plant size described in Table \ref{tab:allometry} (see Supplementary material for derivations).

In a similar way, basal area increment can be expressed as the sum of increments in sapwood, bark \& heartwood area, $\textrm{d}a_\textrm{st} / \textrm{d}t= \textrm{d}a_\textrm{ss} / \textrm{d}t + \textrm{d}a_\textrm{b} / \textrm{d}t + \textrm{d}a_\textrm{sh} / \textrm{d}t$. Sustituting from Table \ref{tab:allometry} gives:
\begin{equation} \label{eq:dast}
\frac{\textrm{d}a_\textrm{st}}{\textrm{d}t}=
\underbrace{\strut \frac{1+b}{\theta} \times
\frac{\textrm{d}a_\textrm{l}}{\textrm{d}m_\textrm{t}} \times \frac{\textrm{d}m_\textrm{t}}{\textrm{d}P} \times \frac{\textrm{d}P}{\textrm{d}t}}_\textrm{expanding sapwood area}
+ \underbrace{\strut k_\textrm{s} \, \theta^{-1} \, a_\textrm{l}}_\textrm{heartwood formation}.
\end{equation}
Diameter growth can also be calculated via the geometric relationship between stem diamater ($D$) and $a_\textrm{st}$:
\begin{equation} \label{eq:dDdt}
\frac{\textrm{d}D}{\textrm{d}t}= \sqrt{\frac{\pi}{a_\textrm{st}}} \frac{\textrm{d}a_\textrm{st}}{\textrm{d}t}.
\end{equation}

Equations \ref{eq:dhdt}, \ref{eq:dast} and \ref{eq:dDdt} ... size dependence...

- Figure: what drives decline at larger sizes --> Amount due to respiration, turnover, architecture, support costs, reproduction
- Growth rate shows hump-shaped pattern with with size. Figure \ref{f-hump}: Hump-shaped curve, 4 components


The direct effect of increasing $\phi$ on growth is to increase the marginal cost of building leaf area (eq. \ref{eq:daldmt}) and thus reduce growth rate. However, to fully account for the influence of $\phi$ on growth we must also account for benefits of superior leaf construction: increased leaf longevity[@wright_world-wide_2004]. This is achieved by linking the turnover rate of leaf in eq. \ref{eq:dPdt} to $\phi$. We use an allometric scaling relationship of the form $k_\textrm{l}=\alpha_4 \, \phi^{\beta_4}$, which has been shown to describe patterns across diverse ecosystems[@wright_world-wide_2004] (Fig. \ref{fS-leaf}).

Unlike eq. \ref{eq:RGR}, eqs. \ref{eq:dhdt}, \ref{eq:dast} and \ref{eq:dDdt} predict a relationship between $\phi$ and growth that changes with plant size. Decreasing $\phi$ has two impacts on growth rate. First, lower $\phi$ increases leaf deployment rate ($\textrm{d}a_\textrm{l} / \textrm{d}m_\textrm{t}$ by economising on construction costs. Second, lower $\phi$ decreases net production ($\textrm{d}P / \textrm{d}t$), due to increased leaf turnover. Whether lower $\phi$ increases growth thus depends on the relative size of these two effects. When plants are small the effect on leaf deployment rate is bigger and so decreasing $\phi$ increases growth rate. When plants are large, the influence of $\phi$ on leaf deployment rate is diminished, because the costs of building other supportive tissues (other terms in eq. \ref{eq:daldmt}) are larger (Fig. XX). The net result is that at larger sizes, low $\phi$ is no longer advantageous for growth.

- Correlation of growth rate with LMA changes with size, Figure \ref{f-LMA_growth_size}
- Figure \ref{f-mass_fraction}: differential effects of LMA on dP/dt and dA/dMt.

A number of other predictions arise directly from this model (Table \ref{tab:predictions}).

- Responsiveness of growth to light changes with LMA. Figure \ref{f-LMA_growth_light}: sensitivity of growth to light
- Low LMA species less shade tolerant Figure \ref{f-LMA_wplcp}: LMA vs wplcp for different size classes
- Shade-tolerance also decreases with size. (As previously recognised by Givnish). see previous figure \ref{f-LMA_wplcp}:
- Faster decline in shade tolerance with size for low LMA species ??
- more than just shade - any prolonged period of low productivity. e.g. low LMA on less productive soils
- Ontogenetic shift towards higher LMA with size. Figure \ref{f-LMA_optim}: LMA maximising growth rate against size for different light environments

Conclusion:

1. Growth rate is size-dependent
2. Strength of trait-correlation varies with size. Could be increasing, decreasing, reversing
3. Species not 'fast' or 'slow', rather have a trajectory
4. We need theory to make sense of data

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

\begin{table}[h]
\caption{Equations for an allometric growth model}
\centering

\begin{tabular}{lll}
\hline
Variable & Function & Derivative\\ \hline
leaf area & $a_\textrm{l}=\alpha_1 \, h^{\beta_1}$ & $\frac{\textrm{d}h}{\textrm{d}a_\textrm{l}}= -\beta_1\big(\frac{a_\textrm{l}}{\alpha_1}\big)^{-(\beta_1+1)}$\\
leaf mass & $m_\textrm{l}=\phi \, a_\textrm{l} $ & $\frac{\textrm{d}m_\textrm{l}}{\textrm{d}a_\textrm{l}}=\phi$\\
sapwood mass&$m_\textrm{s}=\theta^{-1} \, \rho \, \eta_c \, a_\textrm{l} \, h $ & $\frac{\textrm{d}m_\textrm{s}}{\textrm{d}a_\textrm{l}}=\theta^{-1}\, \rho\, \eta_c\, \big( h + a_\textrm{l}\, \frac{\textrm{d}h}{\textrm{d}a_\textrm{l}} \big)$\\
bark mass&$m_\textrm{b}=b\, \theta^{-1} \, \rho \, \eta_c \, a_\textrm{l} \, h $ & $\frac{\textrm{d}m_\textrm{b}}{\textrm{d}a_\textrm{l}}=b \, \theta^{-1} \, \rho \, \eta_c\big( h + a_\textrm{l} \, \frac{\textrm{d}h}{\textrm{d}A} \big)$\\
root mass & $m_\textrm{r}=\alpha_3 \, a_\textrm{l}$ & $\frac{\textrm{d}m_\textrm{r}}{\textrm{d}a_\textrm{l}}= \alpha_3$ \\
sapwoood area & $a_\textrm{ss}=\theta^{-1} \, a_\textrm{l}$ & $\frac{\textrm{d}a_\textrm{ss}}{\textrm{d}t} =\theta^{-1} \, \frac{\textrm{d}a_\textrm{l}}{\textrm{d}t}$\\
bark area & $a_\textrm{sb}=b \, \theta^{-1} \, da_\textrm{l}$ & $\frac{\textrm{d}a_\textrm{sb}}{\textrm{d}t}=b \, \theta^{-1} \, \frac{\textrm{d}a_\textrm{l}}{\textrm{d}t}$\\
heartwood area & $a_\textrm{sh}=\int_0^t \frac{\textrm{d}a_\textrm{sh}}{\textrm{d}t}(t^\prime) \, dt^\prime$ & $\frac{\textrm{d}a_\textrm{sh}}{\textrm{d}t}=k_\textrm{s} \, a_\textrm{ss}$\\\hline
\end{tabular}
\label{tab:allometry}
\end{table}

\newpage

# Figures
FIGURES

![**The relationship between leaf mass per area and growth rate varies with plant size.**
\label{f-RGR}](../figs/RGR.pdf)

\newpage

![**Correlation between LMA and growth changes with plant size.**
\label{f-LMA_growth_size}](../figs/growth-height.pdf)

\newpage

![**Sensitivity of growth to light.**
\label{f-LMA_growth_light}](../figs/growth-light-1.pdf)

\newpage

![**Whole plant light compensation point.**
\label{f-LMA_wplcp}](../figs/LMA_wplcp.pdf)

\newpage

![**LMA maximising growth rate against size for different light environments.**
\label{f-LMA_optim}](../figs/LMA_optim.pdf)



\newpage

# Supplementary material

![**Model assumptions.**
**a)** Across species, leaf mass per area is inversely related to leaf turnover rate. Data from @wright_world-wide_2004 for 678 species from 51 sites. Green lines show standardised major axis lines fitted to data from each site, with intensity of shading adjusted according to strength of the relationship.\label{fS-leaf}](../figs/SI-leaf.pdf)

![**Hump-shaped relationship between growth rate and size.**
\label{f-hump}](../figs/size-dhdt.pdf)

![**Change in allocation with size.**
\label{f-mass_fraction}](../figs/mass_fraction.pdf)

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

**note, scaling relationship reveresed from Falster et al 20011***

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
m_\textrm{s}=\rho \, \int_0^h \, S(z,h) \, \textrm{d}z= \rho \, S(0,h) \, h \, \eta_c,
\end{equation}

where $\eta_c=1-\frac{2}{1+\eta} + \frac{1}{1+2\eta} $[@yokozawa_foliage_1995]. Substituting from eq. \ref{eq:crown2} into eq. \ref{eq:ms1} gives an expression for sapwood mass as a function leaf area and height:

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
