options(mc.cores = min(3, parallel::detectCores()))

data_for_stan <- function(data, gvar, xvar, yvar, n=NULL) {

  data <- data[!is.na(data[[xvar]]*data[[yvar]]),]

  if(!is.null(n)){
    data <- data[seq_len(min(n, nrow(data))),]
  }

  g <- factor(data[[gvar]])
  list(
    n_obs = nrow(data),
    n_groups = length(unique(data[[gvar]])),
    group = as.numeric(g),
    x = as.matrix(data[, c(xvar, yvar)]),
    group_names = levels(g),
    gvar = gvar,
    xvar = xvar,
    yvar = yvar
    )
}

fit_MCMC <- function(data, obj, pars, chains=3, iter=300, ...) {

  rstan_options(auto_write = TRUE)
  sampling(obj,
                         data= data,
                         pars = pars,
                         chains = chains,
                         iter = iter, ...)
}

fit_stan_model <- function(data, model, pars, optim=FALSE, ...) {
  obj <- stan_model(model_code=model)
  fit <- fit_MCMC(data, obj, pars = pars, ...)
  list(fit=fit, data=data, pars=pars)
}

fit_group_stan <- function(data, positive=TRUE,...) {
  stan <- stan_model_group(positive=positive)
  fit_stan_model(data, stan$model,
    unlist(stan$pars, use.names = FALSE),...)
}

stan_model_group <- function(positive=TRUE) {
  list(
    model=sprintf("
  data {
    int<lower=1> n_obs;
    int<lower=1> n_groups;
    int<lower=1> group[n_obs];
    vector[2] x[n_obs];
    }

  parameters {
    # higher-level distributions
    real mu_mu_x;
    real<lower=0> sigma_mu_x;
    real mu_b_0;
    real<lower=0> sigma_b_0;
    real mu_b_1;
    real<lower=0> sigma_b_1;
    real<lower=0> a_sigma_u1;
    real<lower=0> b_sigma_u1;
    real<lower=0> a_sigma_u2;
    real<lower=0> b_sigma_u2;

    # lower-level effects  by group
    real mu_x1[n_groups];
    real b_0[n_groups];
    real<%s=0> b_1[n_groups];
    real<lower=0> sigma_u1[n_groups];
    real<lower=0> sigma_u2[n_groups];
  }

  model {
    # Define variables
    vector[2] uv[n_obs];         # (u1,u2) as vector
    matrix[2,2] U[n_groups];     # Rotation matrices
    vector[2] mu_x[n_groups];    # vector means for x
    vector[2] sigma_u[n_groups]; # cov. matrices for u

    # Sample group-level pars from distributions
    mu_x1 ~ normal(mu_mu_x, sigma_mu_x);
    b_0 ~ normal(mu_b_0, sigma_b_0);
    b_1 ~ normal(mu_b_1, sigma_b_1);
    sigma_u1 ~ normal(a_sigma_u1, b_sigma_u1);
    sigma_u2 ~ normal(a_sigma_u2, b_sigma_u2);

    # For each group, calculate vectors/matrices from pars
    for (i in 1:n_groups) {
      # Means
      mu_x[i, 1] = mu_x1[i];
      mu_x[i, 2] = b_0[i] + b_1[i] * mu_x1[i];

      # Rotation matrix, U
      U[i,1,1] =  b_1[i]^2;
      U[i,1,2] =  b_1[i];
      U[i,2,1] = -b_1[i];
      U[i,2,2] =  1;
      U[i] = (1 / sqrt(2) / fabs(b_1[i])) * U[i];

      # Covariance matrix of uv
      sigma_u[i,1] = sigma_u1[i];
      sigma_u[i,2] = sigma_u2[i];
    }

    # Calculate u by centering then rotating x
    for (j in 1:n_obs) {
      uv[j] = U[group[j]] * (log(x[j]) - mu_x[group[j]]);
    }

    # Likelihood of data
    for (j in 1:n_obs) {
      uv[j] ~ multi_normal(rep_vector(0,2),
                          diag_matrix(sigma_u[group[j]]));
    }
  }", ifelse(positive, "lower", "upper")
  ),
  pars = list(higher = c("mu_mu_x", "sigma_mu_x", "mu_b_0", "sigma_b_0",
           "mu_b_1", "sigma_b_1", "a_sigma_u1", "b_sigma_u1",
           "a_sigma_u2", "b_sigma_u2"),
            lower = c("mu_x1","b_0","b_1","sigma_u1","sigma_u2"))
  )
}

figure_fit_by_group <- function(data, isoclines=NULL, A=NULL,
  xlab="", ylab="", col = make_transparent("grey", 0.4), ...){

  df <- data[["x"]]

  plot(df[,1], df[,2],  type= 'n', log="xy", yaxt="n", xaxt="n",
    xlab= "", ylab= "",
    ...)
  axis.log10(1)
  axis.log10(2)

  if(!is.null(isoclines))
    add_isoclines(A=A, b=isoclines)

  if(!is.null(col))
    points(df[,1], df[,2],  type= 'p', col=col, pch=16)

  mtext(xlab, 1, line=3)
  mtext(ylab, 2, line=3)
}

fit_by_group_stan_summary <- function(fit_stan) {

  # Sampling
  samples <- extract(fit_stan$fit, pars = unlist(fit_stan$pars, use.names = FALSE))
  b_0v = apply(samples$b_0, 2, mean)
  b_1v = apply(samples$b_1, 2, mean)

  dd <- data.frame(X = log(fit_stan$data$x[,1]),
                   Y = log(fit_stan$data$x[,2]),
                   group = fit_stan$data$group) %>%
        mutate(
          group_name = fit_stan$data$group_names[group],
          b_0 = b_0v[group],
          b_1 = b_1v[group],
          f = Y+b_1*X
        ) %>%
        group_by(group) %>%
        summarise(
          group_name = group_name[1],
          b_0=b_0[1],
          b_1=b_1[1],
          from=exp((min(f) - b_0)/(2.0*b_1)),
          to=exp((max(f) - b_0)/(2.0*b_1))
          ) %>%
        mutate(group = as.character(group))
  dd
}

figure_fit_by_group_stan <- function(fit_stan, isoclines=NULL,
  col.p = make_transparent("grey", 0.4),
  col.l = "forestgreen",  ...){

  figure_fit_by_group(fit_stan$data, isoclines,
    col = col.p, ...)

  dd <- fit_by_group_stan_summary(fit_stan)

  for(i in seq_len(nrow(dd)))
    curve(exp(dd$b_0[i])*x^dd$b_1[i], dd$from[i], dd$to[i], add=TRUE,
      col=col.l)
  invisible(dd)
}

add_isoclines <- function(A=NULL, b=NULL,...){
  if(!is.null(A) & ! is.null(b)) {
    b <- 1
  }
  for(a in A) abline(a,b,lty="dotted",...)
}