% The influence of plant size and traits on demographic rates: theoretical expectations
% Daniel Falster

# Introduction
How do plant traits influence growth rate and survival throughout the life-cycle? Existing literature provides several insights, however, a comprehensive understanding of plant growth is lacking. Here we use a mechanistic model of plant allocation and mass production to clarify the likely influences of several key traits on growth and mortality rates. Our analysis builds on previous studies, designed to understand differences in the relative growth rate of seedlings, whereby growth rate is decomposed into several components, each influenced by traits. Unlike the seedling studies, the new model allows the changing influence of traits through ontogeny to be assessed.

Ecologists have identified a variety of traits influencing plant function; the challenge now is to understand how these traits influence growth, mortality and fecundity across the life cycle.

Aim is to make different effects of allometry, allocation and mass production explicit. Need to point our where different traits enter, and how tradeoffs enter elsewhere.

# Model
Let us consider the height growth rate (m yr$^{-1}$), diameter growth rate (m yr$^{-1}$), and mass growth rate (kg yr$^{-1}$). Height growth rate can be decomposed as
\begin{equation} \begin{split}
\frac{dh}{dt} &= \frac{dh}{dA} \times \frac{dA}{dm_t} \times \frac{dm_t}{dt},\\
\end{split} \end{equation}
where $h$ is the height of the plant, $A$ is total leaf area, and $m_t$ is the total mass.

The components of this model are as follows:

- $\frac{dh}{dA}$ is the growth in height per unit growth in leaf area; it accounts for the architectural strategy of the plant. Some species tend to leaf out more than grow tall, while other species emphasise vertical extension.
- $\frac{dA}{dm_t}$ is the rate of leaf area growth per unit mass growth for the plant. It accounts for costs of deploying leaf area, including the leaf itself and the various support structures required.
- $\frac{dm_t}{dt}$ is the mass production rate of the plant. Greater mass production naturally leads to increased growth rates.

Thus height growth rate is given by the product
\begin{equation} \textrm{Height growth} = \textrm{Architectural layout} \times \textrm{Tissue costs} \times \textrm{Net photosynthetic Income}.\end{equation}
Ecologists have identified a variety of traits influencing plant function; the challenge now is to understand how these traits influence growth, mortality and fecundity across the life cycle.

Each of these terms can be further decomposed, allowing the effect of traits to be clarified.

## Tissue costs
The rate of leaf area growth per unit mass growth for the plant is given by
\begin{equation} \frac{dA}{dm_t} = \frac{1}{\frac{dm_l}{dA} + \frac{dm_s}{dA} + \frac{dm_h}{dA} + \frac{dm_r}{dA}} = \frac{1}{\tau_{l} + \tau_{s} + \tau_{h} +\tau_{r}}, \end{equation}
where $\tau$ is the marginal cost of adding leaf ($l$), sapwood and bark ($s$), heartwood ($h$), and root ($r$) tissue per unit leaf area (units kg/m$^2$). Specific functions for these marginal costs are obtained from a mechanistic allocation model. For example, assuming the pipe model, a fixed ratio of root mass per leaf area, and an allometric relation between total leaf area and heartwood volume gives the following equations:


\begin{tabular}{lll}
\hline
Organ & Allometric relation & Marginal cost\\ \hline
leaf & $m_l = \phi A$ & $\tau_{l} = \phi$\\
sapwood &$m_s = \rho \eta_c \theta^{-1} A h(A)$ & $\tau_{s} = \rho \eta_c \theta \left( h(A) + A \frac{dh}{dA} \right)$\\
root & $m_r = \alpha_3 A$ & $\tau_{r} = \alpha_3$\\
heartwood & $m_h = \rho \eta_c \alpha_2 A^{\beta_2}$ & $\tau_{h} = \rho \eta_c \alpha_2 \beta_2 A^{\beta_2-1}$\\ \hline
\end{tabular}


where $\phi$ is LMA, $\rho$ is wood density, $\theta$ is leaf area per sapwood area, $\alpha_3$ is root mass per leaf area, $\eta_c$ is a constant related to canopy shape, and $\alpha_2, \beta_2$ are empirical constants. Notice that $\tau_{s}$ and $\tau_{h}$, the marginal costs of sapwood and heartwood, increase with size. While similar derivations could be pursued using other allocation models, this general feature is likely to be present in all models considered.

## Net photosynthetic Income
Equations for the net mass growth of the plant can be derived based on well established physiology:
\begin{equation} \begin{split}
\frac{dm}{dt} &= c \times\textrm{Yield} \times \left(\textrm{Assimilation} - \textrm{Respiration}\right) - \textrm{Turnover} \\
&= c \times Y \times \left(A \times a_\textrm{l} - \left(m_l(A) r_\textrm{l} + m_s(A) r_\textrm{s} + m_r(A) r_\textrm{r}\right)\right) \\
& - \left(m_l(A) k_\textrm{l} + m_r(A) k_\textrm{r}\right)
\end{split} \end{equation}
where $a_\textrm{l}$ is photosynthetic rate per unit leaf area, $r_l, r_s, r_r$ are respiration rates per unit leaf, sapwood and root mass, and $k_l, k_r$ are turnover of leaf and root tissue. The equations in table above can be substituted in for $m_l, m_r, m_s$. This causes respiration costs of leaf and root to be linear functions of leaf area, whereas the respiration cost of sapwood increases non-linearly with leaf area. Also, note that the turnover rate of leaf is related to LMA as: $k_l = \alpha_5 \phi^\beta_5$.

Missing from this analysis is:

- Allocation to reproduction. Especially important in large plants.
- Carbohydrate storage and use.
- Turnover of side branches.
- Environmental effects, light \& water env.
\end{enumerate}

## Architectural layout
Summary model of architectural layout, characterised by log-log scaling relationship between height and mass of leaf area:
\begin{equation}  h = \alpha_1 A ^{\beta_1},\end{equation}
where $\alpha_1, \beta_1$ are empirical constants. Thus,
\begin{equation} \frac{dh}{dA} = \alpha_1\beta_1 A ^{\beta_1 -1}.\end{equation}

This can be dissected further. Let us warp a column around the plant, whose volume is: $V=\frac{\pi}{4} W^2 H$, where $W$ is width of the canopy. Canopy width typically scales with $H$: $W = \alpha H^\beta$, so $V=\frac{\pi}{4}\alpha^2 H^(2\beta -1)$. Then let us assume the plant occupies a fixed fraction of this volume $f$, and within that leaf has density $D$, so $A = VfD$. Then
\begin{equation} \begin{split}
\frac{dh}{dA} &= \frac{dh}{dV} \times \frac{dV}{dA} \\
&=...
\end{split}
\end{equation}

This allows:

- canopy area to enter as trait, and for us to use data on this where it exists
- links to Remko's model of light interception - shows that leaf density per volume is approximately constant wrt to size (i.e. volume scales linearly with leaf area) but that leaf becomes more clumped as plants get bigger - and thus self-shading and leaf size


## Consistency with seedling studies
The above formulation is consistent with previous studies on seedling growth rates. Turnover rates can typically be ignored for very young seedlings, in which case income becomes
\begin{equation} \begin{split}
\frac{dm_t}{dt} &= \phi \times m_l \times c \times\textrm{Yield} \times \left(a_\textrm{lf} - r_\textrm{lf}\right)\\
& = \phi \times m_l \times \textrm{NAR},
\end{split}\end{equation}
where $\textrm{NAR} = c \times\textrm{Yield} \times \left(a_\textrm{lf} - r_\textrm{lf}\right)$. Then,
\begin{equation}
\textrm{RGR} = \frac{dm_t}{dt} \frac{1}{m_t}= \phi \times \frac{m_l}{m_t} \times \textrm{NAR}
\end{equation}

## Diameter growth
A prediction for diameter growth rates can also be derived. If we are willing to assume some relationship between diameter and height, then diameter growth rate can be expressed as
\begin{equation} \begin{split}
\frac{dD}{dt} = \frac{dD}{dh} \frac{dh}{dt}.
\end{split} \end{equation}

A more thorough derivation is as follows. Note that diameter is related to the total stem cross section $X_t$ as
\begin{equation} \begin{split}
X_t = 0.25 \pi D^2  \Rightarrow D = 2 \sqrt{\frac{X_t}{\pi}}.
\end{split} \end{equation}
Thus diameter increment is directly related to basal area increment:
\begin{equation} \begin{split}
\frac{dD}{dt} = (\pi X_t)^{-0.5} \frac{dX_t}{dt} = \frac{2}{\pi D} \frac{dX_t}{dt}.
\end{split} \end{equation}
Basal area increment in turn is the sum of increments in sapwood, bark \& heartwood area:
\begin{equation} \begin{split}
\frac{dX_t}{dt}= \frac{dX_s}{dt} + \frac{dX_b}{dt} + \frac{dX_h}{dt}.
\end{split} \end{equation}
Sapwood area increment is directly proportional to leaf area increment, following he pipe model:
\begin{equation} \begin{split}
\frac{dX_s}{dt} = \frac{dX_s}{dA} \frac{dA}{dt} = \theta^{-1} \frac{dA}{dt},
\end{split} \end{equation}
with bark area behaving similarly. Heartwood increment however, is proportional to sapwood turnover rate and thus total leaf area
\begin{equation} \begin{split}
\frac{dX_h}{dt} = k_s X_s = k_s \theta^{-1} A.
\end{split} \end{equation}
Thus,
\begin{equation} \begin{split}
\frac{dD}{dt} &= \frac{2}{\pi D} \frac{dX_t}{dt}\\
			 &= \frac{2}{\pi D} \left(\frac{dX_s}{dt} + \frac{dX_b}{dt} + \frac{dX_h}{dt}\right)\\
			 &= \frac{2}{\pi D} \left(\theta^{-1} \frac{dA}{dt} + b \theta^{-1} \frac{dA}{dt} + k_s \theta^{-1} A\right)\\
			 &= \frac{2}{\pi D \theta } \left((1+b) \frac{dA}{dt} + k_s A\right).\\
\end{split} \end{equation}
This equation shows that diameter growth rate is related to height growth rate (both are dependent on $dA/dt$), but with an allometric adjustment and the added element of heartwood formation.

#Analyses

These are some of the phenomena that might be explained by such a model.

## Changing influence of traits on growth rate through ontogeny
Model predicts that LMA has a large influence on growth rates when small, and a weak or non-existent relationship among larger individuals.This is consistent with experimental work on seedling growth rate and large tress\cite{poorter_are_2008, wright_functional_2010}. In contrast, wood density is predicted to have a small influence on the growth rates of seedlings, but a large influence on growth rates of adult plants. Thus WD is trait with largest effect on adult diameter growth rates, as observed. (See also for data on interactions between size, growth rate and lma \cite{kunstler_size-dependence_2009})

Both patterns result from changes in the marginal cost of sapwood with increasing size. Neither low LMA or low wood density provide any benefits in generating income (in fact, low LMA decreases income because of increased leaf turnover); the benefits of these traits on growth comes from lowering tissue costs. As a plant increases in size, the marginal cost of sapwood increases, such that the marginal cost of leaf tissue - given by LMA - becomes increasingly important. At some point, the costs having low LMA (i.e. high leaf turnover) become sufficiently large to completely offset any advantage in cheap leaf deployment.

Plot $\frac{dA}{dm}$ wrt to size, separating out leaf, sapwood, heartwood components.

More generally, we could assess sensitivity of growth rate to different traits at given size and light environment. Take known range of parameter values (mean plus S.D) and generate expected growth rates, assuming traits are independent. Then plot variance contributed by each trait as function of size, with different traits laid on top of each other to give total variance (=1). This would show the influence of leaf traits decreasing as the plant increases in size, and the influence of stem traits increasing. Could do this as simulation, but better to use equations to do a variance decomposition.

- when small, lma and Nmass (leaf characteristics) have largest effect
- when large, stem characteristics more important (wood density, because it is largest)


## Ontogenetic trend in LMA with size
Widely observed, e.g. \cite{thomas_photosynthetic_2010}.
This is because benefits of low LMA decline with size, so only left with cost. Thus all species predicted to increase LMA as get larger.

Show lma maximising growth rate increases with size $\Rightarrow$ explains ontogenetic pattern in lma with size.

## Link between LMA and shade tolerance
Low lma species have high leaf turnover, which influences mass production and thereby mortality. (Direct link is with LL, which explains why LL better correlated with shade tolerance)

Show lma maximising growth decreases with canopy openness $\Rightarrow$ late successional have higher LMA (or leaf lifespan).

## Change in whole plant light compensation point with size
WPLCP changes with size, because respiration and turnover influences mass production and thereby mortality.

## Declining RGR with size
Arising directly from change in marginal costs with size.

## Different correlations between growth rate and mortality rate within and across species
Positive correlation between growth rate and mortality within species, but negative correlation across species

## Consistency with metabolic scaling

Show that model also captures intrinsic size effect described by metabolic scaling. However, more mechanistic detail.

## Plasticity

Calculate elasticity for growth rate and WPLCP at different sizes. Compare these to observed range of trait variation --> elasticity tells you sensitivity to % change in each traits. Combine this with observed variation in traits. Suggests respiration per leaf area most important. --> can predict which traits most likely to show plasticity in rates in low light.


# Discussion
Use opportunity to emphasise unknowns and future directions.

- Wood density vs stem respiration. If respiration increases with wood density then model actually predicts dens wood spp are more shad tolerant.
- how to incorporate other traits.


# Useful refs

- Alvarez-Buylla1992, King 2011, Herault 2011 has plot of growth rate vs size showing hump shape
