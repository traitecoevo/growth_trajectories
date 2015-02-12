#  Why do trait link to shade tolerance?

##  Production

\begin{equation}
\underbrace{\strut \frac{dP}{\textrm{d}t}}_\textrm{net biomass production}= \underbrace{\strut Y}_\textrm{yield} \big(\underbrace{\strut  A}_\textrm{photosynthesis} - \underbrace{\strut R}_\textrm{respiration}\big) - \big(\underbrace{\strut \sum_\textrm{i=l,b,s,r}{m_\textrm{i} \, k_\textrm{i}}}_\textrm{turnover}\big).
\end{equation}

where

- subscripts $i$ $\rightarrow$ $l$=leaves,
$b$=bark, $s$=sapwood and $r$=roots.
- $m_i$ is mass of tissue
- $k_i$ is turnover rate of tissue

## LMA

Assume relationship between lma and leaf turonver rater, $k_l$:
$$k_\textrm{l}=\alpha_4 \, \phi^{-\beta_4}$$

Then can write

\begin{equation}\label{eq:dPdt2}
\frac{dP}{\textrm{d}t} = c_2 - a_\textrm{l} a_4 \phi ^{1-B_4}
\end{equation}
where
$c_2$ is independent of $\phi$ and $a_l$ is leaf area.

So if $B_4=1$, $\frac{ \textrm{d}P} {\textrm{d}t}$ is independent
of $\phi$.

In contrast, if $B4>1$ (which tends to be the case empirically), $\frac{ \textrm
{d}P} {\textrm{d}t}$ increases as $\phi$ increases.

## Wood density

Assume relationship between wd and sapwood turonver rater, $k_s$:
$$k_\textrm{s}=\alpha_5 \, \rho^{-\beta_5}$$


So if $B_5=1$, $\frac{ \textrm{d}P} {\textrm{d}t}$ is independent
of $\rho$.

In contrast, if $B_5>1$ (which we have no evidence for), $\frac{ \textrm
{d}P} {\textrm{d}t}$ increases as $\rho$ increases.

## Potentially explains link between traits and shade tolerance

## Shade tolerance is key for coexistence


\newpage

# Why does the influence of traits$^*$ on growth change with size?

$^*$ Here focussing on LMA and wood density.

$$G = \underbrace{\strut\frac{\textrm{d}h}{\textrm{d}t}}_{\textrm{height growth rate}}= \underbrace{\strut\frac{\textrm{d}h}{\textrm{d}a_{\textrm{l}}}}_{\textrm{architecture}}
\times \underbrace{\strut\frac{\textrm{d}a_{\textrm{l}}}{\textrm{d}m_{\textrm{t}}}}_{\textrm{leaf deployment}}
\times \underbrace{\strut\frac{\textrm{d}m_{\textrm{t}}}{\textrm{d}P}}_{\textrm{allocation to growth}}
\times \underbrace{\strut\frac{\textrm{d}P}{\textrm{d}t}}_{\textrm{mass production}}$$

where $h$ is height, $a_l$ is leaf area, $m_t$ is total mass, $P$ is net production

Traits only influence two parts, can thus write as


$$G = c_1   \left(\frac{\textrm{d}a_{\textrm{l}}}{\textrm{d}m_{\textrm{t}}}  \frac{ \textrm{d}P} {\textrm{d}t} \right)$$

Now take deriavtive with respect to trait $x$

\begin{equation} \label{eq:dG}
\frac{\partial G} {\partial x} =
\frac{\partial \left(\frac{\textrm{d}a_\textrm{l}} {\textrm{d}m_\textrm{t}}\right)}{\partial x}
 \, \frac{ \textrm{d}P} {\textrm{d}t}
+ \frac{\textrm{d}a_\textrm{l}} {\textrm{d}m_\textrm{t}}
\, \frac{\partial \left( \frac{ \textrm{d}P} {\textrm{d}t}\right)}{\partial x}.
\end{equation}

Want to know when this is

- $> 0$ $\Rightarrow$ growth positively correlated with trait $x$
- $=0$ $\Rightarrow$ growth not correlated with trait $x$
- $< 0$ $\Rightarrow$  growth negatively correlated with trait $x$

Magically reformalte as

\begin{equation} \label{eq:dG2}
\frac{\partial G} {\partial x} =
\frac{\textrm{d}a_\textrm{l}} {\textrm{d}m_\textrm{t}}
\left(
\frac{\partial \left( \frac{ \textrm{d}P} {\textrm{d}t}\right)}{\partial x}
- \frac{\textrm{d}a_\textrm{l}} {\textrm{d}m_\textrm{t}}
\,  \frac{\partial \left(\frac{\textrm{d}m_\textrm{t}} {\textrm{d}a_\textrm{l}}\right)
}{\partial x}
 \, \frac{ \textrm{d}P} {\textrm{d}t}
\right).
\end{equation}

So negative if

\begin{equation}\label{eq:dg4}
\frac{
\frac{\partial \left( \frac{ \textrm{d}P} {\textrm{d}t}\right)}{\partial x} }
{\frac{ \textrm{d}P} {\textrm{d}t}}
<
\frac{ \frac{\partial \left(\frac{\textrm{d}m_\textrm{t}} {\textrm{d}a_\textrm{l}}\right)
}{\partial x}}{\frac{\textrm{d}m_\textrm{t}} {\textrm{d}a_\textrm{l}}}
\, \, \, \,  \textrm{or equivalently} \, \, \, \,
\frac{\partial \log \frac{ \textrm{d}P} {\textrm{d}t}}{\partial x}
<
\frac{\partial \log \frac{\textrm{d}m_\textrm{t}} {\textrm{d}a_\textrm{l}}
}{\partial x}.
\end{equation}

## For LMA

Left hand side of eq. \ref{eq:dg4} is
$$\frac{(B_4-1) a_\textrm{l} a_4\phi ^{-B_4}}{c_2 - a_\textrm{l} a_4 \phi ^{1-B_4}}$$

i.e. if $B_4 =1$ it is 0. So, ineuqaulity is always true, $\rightarrow$ growth increases with lower LMA.

But if $B_4 >1$ see transition at larger sizes.

## For Wood density

Left hand side of eq. \ref{eq:dg4} is
$$\frac{(B_5-1) \eta_c a_\textrm{l} h \theta^{-1}  a_5\rho ^{-B_5}}{c_3 - \eta_c a_\textrm{l} h \theta^{-1}  a_5 \rho ^{1-B_5}}$$

i.e. if $B_5 =1$ it is 0. So, ineuqaulity is always true, $\rightarrow$ growth increases with lower wood density.

But if $B_5 >1$ see transition at larger sizes.
