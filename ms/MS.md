---
title:  'Untangling the link between traits, size and growth rate in plants'
author:
- name: Daniel Falster
  affiliation: Biological Sciences, Macquarie University
  email: daniel.falster@mq.edu.au
- name: Rich FitzJohn
  affiliation: Biological Sciences, Macquarie University
  email: richard.fitzjohn@mq.edu.au
- name: Joe Wright
  affiliation: Center for Tropical Forest Science, Smithsonian Tropical Research Institute, Panama, Republic of Panama
  email: wrightj@si.edu
bibliography: refs.bib
csl: nature.csl
---

# Abstract

# Main text

Plant growth is the foundation of the terrestrial biosphere, powering food-webs and much of human activity[@raupach_carbon_2010]. While most higher plants have the same basic resource requirements (light water, nutrients) and physiological function, large differences exist among species in the amount of energy invested in different tissues (leaves, stems, roots) and the quality with which these are constructed[@westoby_plant_2002;@niklas_evolution_2000]. How -- and even whether -- these *traits* influence the growth of seedlings, saplings and trees is hotly debated. Prominent traits, such as leaf-construction cost ($\phi$) are widely thought to be indicators of potential growth[@enquist_general_2007], because cheaper leaf construction enables faster canopy expansion[@blackman_compound_1919; @westoby_time_2000]. Indeed, growth rate in seedlings is strongly and negatively correlated with $\phi$[@cornelissen_seedling_1996; @wright_cross-species_2000] (Fig. \ref{f-BCI}a). Although, it has been suggested that low $\phi$ might also give a growth advantage in larger plants[@enquist_general_2007], an increasing number of empirical studies have failed to detect any such effect in small or large plants[@wright_functional_2010, @herault_functional_2011, @poorter_are_2008] (Fig. \ref{f-BCI}c). Meanwhile, expectations on how other prominent traits might influence growth are lacking. The growing discord between theoretical expectation and empirical result thus indicates a strong need for a new theoretical framework through which the influence of traits and size on plant growth can be understood.

Here we extend a widely-used theoretical model that links growth rate in seedlings to $\phi$[@lambers_inherent_1992; @wright_cross-species_2000], to explicitly include influences of size, light environment, and other prominent traits. This extension allows us to generate novel predictions on the link between four key traits and plant demography, in particular why the effect of each trait on growth is moderated by plant size (Table \ref{tab:predictions}). We then verify these predictions using over 30 years of growth data collected at Barro Colorado Island, Panama. In line with model predictions, we show that the effect of traits on growth changes with plant size, with different traits exhibiting distinct size-dependent effects (Fig. \ref{f-BCI}). The effect of $\phi$ is strongest in small plants and absent in large plants, while the influence of height at maturation ($h_m$) shows the opposite pattern. Meanwhile, wood-construction cost ($\rho$) has limited influence in seedlings, but a strong effect in saplings and large trees  (Fig. \ref{f-BCI}).(TODO: Add Leaf N and Sapwood area per leaf area). Together, these findings unify a large number of otherwise disparate empirical findings about plant growth and its link to traits.

We begin with a standard model for the amount of biomass available for growth, $dP / \textrm{d}t$, given by the difference between income (total photosynthesis) and losses (respiration and turnover) within the plant[@falster_influence_2011]:
\begin{equation}\label{eq:dPdt}
\frac{dP}{\textrm{d}t}= \underbrace{\strut Y}_\textrm{yield} \big(\underbrace{\strut \frac{m_\textrm{l}}{\phi} \, A}_\textrm{photosynthesis} - \big(\underbrace{\strut \, \frac{m_\textrm{l}}{\phi} \, r_\textrm{l} + \sum_\textrm{i=b,s,r}{m_\textrm{i} \, r_\textrm{i}}}_\textrm{respiration}\big)\big) - \big(\underbrace{\strut \sum_\textrm{i=l,b,s,r}{m_\textrm{i} \, k_\textrm{i}}}_\textrm{turnover}\big).
\end{equation}
Here, $m,r$, and $k$ refer to the mass, respiration rate, and turnover rate of different tissues, denoted by subscripts $l$=leaves, $b$=bark, $s$=sapwood and $r$=roots. $A$ is the assimilation rate per leaf area and $Y$ is yield, which accounts for growth respiration (see Table \ref{tab:params} for units and definitions). Photosynthesis is proportional to leaf area, $a_\textrm{l} = m_\textrm{l} / \phi$, while total mass of living tissues $m_\textrm{t}=m_\textrm{l}+m_\textrm{b}+m_\textrm{s}+m_\textrm{r}.$ Eq. \ref{eq:dPdt} assumes that tissues lost via turnover are replaced before new growth occurs.

Current theory emphasises the relationship between $\phi$ and relative growth in mass[@enquist_general_2007], $\textrm{d}P / \textrm{d}t / m_\textrm{t}$. For seedlings, which are young and mostly leaf, it is reasonable to ignore all turnover terms as well as the respiration terms for non-leaf tissues. Net production then becomes a linear function of leaf area and net photosynthesis per leaf area ($A_\textrm{net} = Y(A - r_\textrm{l})$), making relative growth rate a linear function of $\phi$:
\begin{equation}\label{eq:RGR}
\frac{\textrm{d}P}{\textrm{d}t} \frac{1}{m_\textrm{t}} \approx A_\textrm{net} \times \phi^{-1} \times \frac{m_\textrm{l}}{m_\textrm{t}}. \end{equation}
Although Eq. \ref{eq:RGR} captures patterns of growth in seedlings in relation to $\phi$[@wright_cross-species_2000], this approximation does not directly link to other traits, or to the variables that are routinely collected for large trees: namely plant height ($h$) and stem cross-sectional area ($a_\textrm{st}$) or diameter $D$.

To model growth in either height or basal area requires we to account not just for mass production, but also for the costs of building new tissues, allocation to reproduction, and architectural layout. In eq. \ref{eq:RGR}), these terms are missing, whereas in the model presented here they are made explicit.

Mathematically, height growth can be decomposed into a product of physiologically relevant terms[@falster_influence_2011]:
\begin{equation} \label{eq:dhdt}
\underbrace{\strut\frac{\textrm{d}h}{\textrm{d}t}}_\textrm{height growth rate}= \underbrace{\strut\frac{\textrm{d}h}{\textrm{d}a_\textrm{l}}}_\textrm{architecture}
\times \underbrace{\strut\frac{\textrm{d}a_\textrm{l}}{\textrm{d}m_\textrm{t}}}_\textrm{marginal leaf deployment}
\times \underbrace{\strut\frac{\textrm{d}m_\textrm{t}}{\textrm{d}P}}_\textrm{allocation to growth}
\times \underbrace{\strut\frac{\textrm{d}P}{\textrm{d}t}}_\textrm{mass production}.
\end{equation}

The first term on the RHS of eq \ref{eq:dhdt}, $\textrm{d}h / \textrm{d}a_\textrm{l}$, is the growth in plant height per unit growth in total leaf area; accounting for the architectural strategy of the plant. Some species tend to leaf out more than grow tall, while other species emphasise vertical extension[@poorter_architecture_2006].

The second term, $\textrm{d}a_\textrm{l} / \textrm{d}m_\textrm{t}$, accounts for the marginal cost of deploying an additional unit of leaf area, including construction of the leaf itself and various support structures. As such, $\textrm{d}a_\textrm{l} / \textrm{d}m_\textrm{t}$ can itself be expressed as a sum of construction costs per unit leaf area:
\begin{equation}\label{eq:daldmt}
\frac{\textrm{d}a_\textrm{l}}{\textrm{d}m_\textrm{t}}
= \big(\phi
 + \frac{\textrm{d}m_\textrm{s}}{\textrm{d}a_\textrm{l}} + \frac{\textrm{d}m_\textrm{b}}{\textrm{d}a_\textrm{l}} + \frac{\textrm{d}m_\textrm{r}}{\textrm{d}a_\textrm{l}}\big)^{-1}.
\end{equation}

The third term in eq \ref{eq:dhdt}, $\textrm{d}m_\textrm{t} / \textrm{d}P$, gives the fraction of net production ($\textrm{d}P / \textrm{d}t$ from eq. \ref{eq:dPdt}) that is allocated to growth rather than reproduction or storage.

In a similar way, basal area ($a_\textrm{st}$) increment can be expressed as the sum of increments in sapwood, bark \& heartwood areas ($a_\textrm{ss}, a_\textrm{sb}, a_\textrm{sh}$ respectively). Noting that $a_\textrm{st}=a_\textrm{ss} + a_\textrm{sb} + a_\textrm{sh}$ and that heartwood grows via turnover of sapwood, we have:
\begin{equation}\label{eq:dast}
\frac{\textrm{d}a_\textrm{st}}{\textrm{d}t}=
\underbrace{\strut \left(\frac{\textrm{d}a_\textrm{ss}}{\textrm{d}a_\textrm{l}} + \frac{\textrm{d}a_\textrm{sb}}{\textrm{d}a_\textrm{l}}\right) \times
\frac{\textrm{d}a_\textrm{l}}{\textrm{d}m_\textrm{t}} \times \frac{\textrm{d}m_\textrm{t}}{\textrm{d}P} \times \frac{\textrm{d}P}{\textrm{d}t}}_\textrm{expanding sapwood and bark area}
+ \underbrace{\strut k_\textrm{s} a_\textrm{ss}}_\textrm{heartwood formation}.
\end{equation}

Diameter growth is then given by the geometric relationship between stem diameter ($D$) and $a_\textrm{st}$:
\begin{equation} \label{eq:dDdt}
\frac{\textrm{d}D}{\textrm{d}t}= \sqrt{\frac{\pi}{a_\textrm{st}}} \frac{\textrm{d}a_\textrm{st}}{\textrm{d}t}.
\end{equation}

Eqs. \ref{eq:dhdt}-\ref{eq:dDdt} are mathematically true, and must therefore hold for any model of plant growth. To make explicit predictions, however, requires two additional elements: 1) A specific model describing how the various mass and area terms ($m_\textrm{l}, m_\textrm{s}, m_\textrm{b}, m_\textrm{r}, a_\textrm{l}, a_\textrm{s}, a_\textrm{b}$) vary relative to one another; and 2) For any trait-mediated trade-offs in plant function to be specified.

To address the first of these challenges, we use a functional balance model[@yokozawa_foliage_1995; @falster_influence_2011] assuming 1) an allometric relation between total leaf area and plant height, and 2) a constant ratio between total leaf area and each of sapwood cross-sectional area, bark cross-sectional area, and total root mass. These assumptions, which are well-supported by data (Fig. \ref{f-assumptions}), lead immediately to an allometric model for plant size (Table \ref{tab:allometry}, see Supplementary material for derivations). Substituting from Table \ref{tab:allometry} into eqs. \ref{eq:dhdt}, \ref{eq:dast}, \ref{eq:dDdt} then gives an explicit model for plant growth in relation to size.

This new growth model captures the intrinsically size-dependent nature of plant growth (Table \ref{tab:predictions}a), recovering well-known empirical patterns[@sillett_increasing_2010; king_size-related_2011]. Height growth shows a hump-shaped pattern with size, first increasing then decreasing. This pattern results from systematic changes in the four components of eq. \ref{eq:dhdt}  with size (Fig. \ref{f-hump}), including a strong decline in the fraction of plant that is leaf declines with increasing size (Fig. \ref{f-mass_fraction}), and increasing reproductive allocation. In contrast, basal-area growth continues to increase with size[@sillett_increasing_2010, @stephenson_rate_2014], due to an increasing influence of stem turnover. Diameter growth shows a weakly hump shaped curve[@herault_functional_2011], tapering off slightly at larger sizes, in part because of allometric effects (eq. \ref{eq:dDdt}, but mainly because of increased reproductive allocation in older trees (Fig?). All growth measures decrease sharply with size when expressed as relative growth rates[@iida_linking_2014].

We now incorporate the effects of some prominent traits by embedding explicit trade-offs within this growth model. Variation in each trait implies benefits and costs. These affect specific elements of the growth equations presented above (Table \ref{tab:trade-offs}).

By definition, the main effect of increasing $\phi$ is to decrease the marginal leaf deployment per mass (eq. \ref{eq:daldmt}) and thus reduce growth rate. However, to fully account for the influence of $\phi$ on growth we must also account for the decrease in leaf turnover that results from superior leaf construction[@wright_world-wide_2004]. This is achieved by linking the turnover rate of leaf in eq. \ref{eq:dPdt} to $\phi$. We use an allometric scaling relationship of the form $k_\textrm{l}=\alpha_4 \, \phi^{\beta_4}$, which has been shown to describe patterns across diverse ecosystems[@wright_world-wide_2004] (Fig. \ref{fS-leaf}).

Unlike eq. \ref{eq:RGR}, eqs. \ref{eq:dhdt}, \ref{eq:dast} and \ref{eq:dDdt} predict a relationship between $\phi$ and growth that changes with plant size (Figs. \ref{f-BCI}, \ref{f-lma_growth_size}). Decreasing $\phi$ has two impacts on growth rate. First, lower $\phi$ increases marginal leaf deployment ($\textrm{d}a_\textrm{l} / \textrm{d}m_\textrm{t}$ by economising on construction costs. Second, lower $\phi$ decreases net production ($\textrm{d}P / \textrm{d}t$), due to increased leaf turnover. Whether lower $\phi$ increases growth thus depends on the relative size of these two effects. When plants are small the effect on leaf deployment rate is bigger and so decreasing $\phi$ increases growth rate. When plants are large, the influence of $\phi$ on leaf deployment rate is diminished, because the costs of building other supportive tissues (other terms in eq. \ref{eq:daldmt}) are larger (Fig. \ref{f-mass_fraction}). The net result is that at larger sizes, low $\phi$ is no longer advantageous for growth (Fig. \ref{f-lma_growth_size}d).

As for leaf, cheaper wood construction (lower $\rho$) increases marginal leaf deployment per mass and may thereby increase growth rates. However, the direct physiological trade-offs of lower $\rho$ are less understood than for $\phi$. One possibility is that lower $\rho$ decreases mechanical strength resulting in higher mortality[@chave_towards_2009; @wright_functional_2010]. Under this scenario, decreasing $\rho$ will always provide a growth advantage, because the costs of low $\rho$ are not realised within the terms of eqs. \ref{eq:dhdt} and \ref{eq:dast} (Table \ref{tab:trade-offs}). Alternatively, lower $\rho$ may increase turnover of sapwood. Under this scenario, decreasing $\rho$ provides an advantage in  height growth only when the benefits of low $\rho$ outweigh the costs. This effect is strongest at XXX sizes. By contrast, stem basal-area and diameter growth increases with lower $\rho$, because high sapwood turnover also contributes to this growth measure. Thus growth continues, even as net production approaches zero.

Greater height at maturation ($h_m$) leads to a growth advantage among larger plants by reducing the amount of energy invested in reproduction, thus increasing $\frac{\textrm{d}m_\textrm{t}}{\textrm{d}P}$ in eqs. \ref{eq:dhdt} and \ref{eq:dast}. At larger sizes, individuals of some species are allocating a majority of their surplus energy to growth, leaving a strong signal in potential growth rate. At smaller sizes, this differentiation disappears (Fig. \ref{f-BCI}), as predicted (Table \ref{tab:predictions}).

Leaf N

- Increases Amax and respiration rate
- no size effect (need to verify)?

Leaf area per sapwood area

- expect growth advantage btu interaction with size unclear
- see also @westoby_evolutionary_2012

In the current model, the only effects of the trait seed size ($s$) are to influence starting size of seedlings and fecundity of the mother. Seed size therefore has no direct influence on absolute growth (Table \ref{tab:predictions}). In may studies seed size has been found to be negatively correlated with mass-based relative growth rate of seedlings. This effect is recovered here because of the intrinsic decrease in RGR with size.

Two other important predictions arise directly from linking traits to growth rate (Table \ref{tab:predictions}c). First, species with low construction cost show stronger responses to changes in light environment. (XXX check this is true with respect to to sixe). Fig. \ref{f-lma_growth_light} shows that species with low $\phi$ exhibit a wider range of growth rates, and also that the relationship between $\phi$ and growth rate flattens out under more stressed conditions. A similar response is observed for changes in $\rho$, matching empirically observed patterns[@ruger_functional_2012].

Second, species with low $\phi$ and low $\rho$ are predicted to be less shade tolerant (Fig. \ref{f-lma_wplcp}). At low $\phi$ ($\rho$), leaf (sapwood) turnover is higher and thus a greater light income is needed to offset these costs. As previously suggested [@givnish_adaptation_1988], shade-tolerance also decreases with height because as size increase, the total amount of energy needed to offset respiratory and turnover costs in the stem also increases. These finding matches well known empirical patterns, where both $\phi$ and $\rho$ have been liked to shade tolerance[@poorter_leaf_2006; @lusk_ontogenetic_2008; @osunkoya_light_1996].

Conclusion,.....


# Methods

Analysis of BCI data.

\newpage

# Figures

![**The relationship between traits and growth rate varies with plant size.**
For XXX species growing in lowland forest at BCI Panama, we estimated the potential growth rate of individual's in that species at a given size and plotted this against four prominent traits. The size of circles in each panel indicates the number of data points used to estimate potential growth rate. \label{f-BCI}](figures/BCI_data.pdf)

\newpage

![**Key assumptions of allometric model.**
\label{f-assumptions}](../figs/allometry.pdf)

\newpage

![**Low construction cost allows more vigorous response to increased light.**
Plot shows predicted relationship between height growth rate and leaf-construction cost under a range of shading environments for plants 0.25m tall.
\label{f-lma_growth_light}](figures/growth_light.pdf)

\newpage

![**Low construction cost leads to shade intolerance, because of costs of high turnover.**
\label{f-wplcp}](figures/lma_wplcp.pdf)

\newpage

# Tables

\begin{table}[H]
\caption{Model predictions and evidence.}

{\footnotesize
\centering
\begin{tabular}{p{4cm}p{4cm}p{2cm}p{6cm}}
\\
\multicolumn{4}{l}{\textbf{a) Intrinsic changes in demography with size}} \\ \\
\hline
Variable & Change with size & & Support \\ \hline
Height growth & strongly hump-shaped & & Model: ; Data:  \\
Diameter growth & weakly hump-shaped & & \\
Basal area growth & increases & & \\
Live mass growth & weakly hump-shaped & & \\
ABG mass growth & increases & & \\
Relative growth (any) & decreases & & \\
Shade tolerance & decreases & & \\ \\
\end{tabular}

\begin{tabular}{p{4cm}p{2.5cm}p{3.5cm}p{6cm}}
\multicolumn{4}{l}{\textbf{b) Relationship of demography to traits}} \\ \\
\hline
Variable & When small & Change with size & Support \\ \hline
\multicolumn{4}{l}{\emph{Leaf-construction cost}} \\

Height growth & negative & $\circlearrowleft$  & \\
Diameter growth & negative & flattens & \\
Basal area growth & negative & $\circlearrowleft$ & \\
Live mass growth & ??  & & \\
ABG mass growth & negative & same & \\
Relative growth (any) & negative & weakens & \\
Shade tolerance & positive &  strengthens & \\ \\

\multicolumn{4}{l}{\emph{Wood-construction cost}} \\

Height growth & negative & strengthens  & \\
Diameter growth & negative & strengthens & \\
Basal area growth & negative & strengthens & \\
Live mass growth & ??  & & \\
ABG mass growth & negative & strengthens & \\
Relative growth (any) & negative & ?? & \\
Shade tolerance & ?? &  ?? & \\ \\

\multicolumn{4}{l}{\emph{Height at maturation}} \\

Any growth measure & none & becomes positive  & \\
Shade tolerance & none &  none &  \\  \\

\multicolumn{4}{l}{\emph{Leaf-nitrogen per area}} \\

Height growth & ?? & ??  & \\
Diameter growth & ?? & ??  & \\
Basal area growth & ?? & ??  & \\
Live mass growth & ?? & ??  & \\
ABG mass growth & ?? & ??  & \\
Relative growth & ?? & ??  & \\
Shade tolerance & ?? &  ?? & \\ \\

\end{tabular}

\begin{tabular}{p{4cm}p{6cm}p{6cm}}
\multicolumn{3}{l}{\textbf{c) Sensitivity of growth strategies to light}} \\ \\
\hline
Trait & Response & Support \\ \hline
Leaf construction & & \\
wood-construction cost & & \\
Height at maturation & & \\
Leaf-nitrogen per area & &\\
\end{tabular}
}
\label{tab:predictions}
\end{table}


\begin{table}[H]
\caption{Equations for an allometric growth model}
\centering

\begin{tabular}{p{5cm}p{5cm}p{5cm} }
\\ \hline
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

\begin{table}[H]
\caption{Trait mediated trade-offs}
\begin{tabular}[c]{l|ccccc|c|c|c}
\multicolumn{9}{c}{}\\ \hline
\textbf{Trait} & \multicolumn{5}{c|}{\textbf{Growth}} & \textbf{Mortality}& \textbf{Fecundity}& \textbf{Birth}\\
& $\frac{\textrm{d}a_\textrm{l}}{\textrm{d}m_\textrm{t}}$
& $\frac{\textrm{d}m_\textrm{t}}{\textrm{d}P}$
& $\frac{\textrm{d}P}{\textrm{d}t}$
& $\frac{\textrm{d}a_\textrm{ss}}{\textrm{d}a_\textrm{l}}$
& $\frac{1}{k_s}$ & & & $h_0$ \\\hline
Leaf-construction cost, $\phi$ & $\downarrow$ & $\uparrow$ & & & & & \\
Wood-construction cost, $\rho$ & $\downarrow$ &  & & & $\downarrow$ & & \\
Leaf area per sapwood area, $\theta$ & $\uparrow$& & $\downarrow$ & $\downarrow$ & & &\\
Leaf-nitrogen & &$\downarrow$$\uparrow$ & & & & & \\
Height at maturation, $h_m$ & &$\uparrow$ & & & & & $\downarrow$ & \\
Seed size, $s$ & & & & & & & $\downarrow$ & $\uparrow$\\ \hline
\end{tabular}
\label{tab:trade-offs}
\end{table}

\newpage


# Supplementary material

##  Derivation of a simple allometric model of plant function

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

\newpage

## Supplementary figures

![**Leaf turnover decreases with leaf-construction cost.**
Data from @wright_world-wide_2004 for 678 species from 51 sites, each point giving a species-average. Lines show standardised major axis lines fitted to data from each site, with intensity of shading adjusted according to strength of the relationship.\label{fS-leaf}](figures/SI_lma_tradeoff.pdf)

\newpage

![**Hump-shaped relationship between growth rate and size.**
\label{f-hump}](figures/SI_size_dhdt.pdf)

\newpage

![**Change in allocation with size.**
\label{f-mass_fraction}](figures/SI_mass_fraction.pdf)

\newpage

![**The expected correlation between leaf-construction cost and growth rate changes with plant size.**
Predictions from the model on the relationship between leaf-construction cost and height growth rate under ideal conditions at a range of sizes.
\label{f-lma_growth_size}](figures/SI_lma_effects_at_diameters.pdf)


\newpage

## Supplementary tables

\begin{table}[H]
\caption{Model parameters}
\centering

\include{table-pars}

\label{tab:params}
\end{table}

\newpage

# References
