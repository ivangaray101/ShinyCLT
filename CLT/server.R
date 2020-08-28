library(shiny)
library(ggplot2)

server = function(input, output){
  
  output$lambda = renderUI({
    if(input$dist == 'rpois' || input$dist == 'rgamma' || input$dist == 'rexp')
    {
      sliderInput(inputId = 'lambda', 
                  label = withMathJax('$$ \\lambda : $$'), 
                  value = 1, 
                  min = 0.001,
                  max = 20)
    }
  })
  
  output$minmax = renderUI({
    if(input$dist == 'runif')
    {
      sliderInput(inputId = 'minmax',
                  label = 'Extremos: ',
                  value = c(0,1),
                  min = -5,
                  max = 5)
    }
  })
  
  output$p = renderUI({
    if(input$dist == 'rgeom' || input$dist == 'rbinom')
    {
      sliderInput('p',
                  withMathJax('$$ p : $$'),
                  value = 0.5,
                  min = 0.001,
                  max = 0.999)
    }
  })
  
  output$n = renderUI({
    if(input$dist == 'rbinom')
    {
      sliderInput('n',
                  withMathJax('$$ n : $$'),
                  value = 1,
                  min = 1,
                  max = 30,
                  step = 1)
    }
  })
  
  output$a = renderUI({
    if(input$dist == 'rbeta' || input$dist == 'rgamma')
    {
      sliderInput('a',
                  withMathJax('$$ a : $$'),
                  value = 1,
                  min = 0.01,
                  max = 10)
    }
  })
  
  output$b = renderUI({
    if(input$dist == 'rbeta')
    {
      sliderInput('b',
                  withMathJax('$$ b : $$'),
                  value = 1,
                  min = 0.01,
                  max = 10)
    }
  })
  
  output$mu = renderUI({
    if(input$dist == 'rnorm')
    {
      sliderInput('mu',
                  withMathJax('$$\\mu :$$'),
                  value = 1,
                  min = -10,
                  max = 10)
    }
  })
  
  output$sigma = renderUI({
    if(input$dist == 'rnorm')
    {
      sliderInput('sigma',
                  withMathJax('$$\\sigma :$$'),
                  value = 1,
                  min = 0.0001,
                  max = 20)
    }
  }) 
  
  output$df = renderUI({
    if(input$dist == 'rchisq')
    {
      sliderInput('df',
                  withMathJax('$$\\nu : $$'),
                  value = 1,
                  min = 1,
                  max = 20,
                  step = 1)
    }
  })
  
  
  poblacion = reactive({
    
    # When you run the App for the first time there is an error, the parameters are null.
    # With !is.null you can solve the problem.
    
    pob_size = input$pob_size
    if(input$dist == 'runif' && !is.null(input$minmax))
    {
      min = input$minmax[1]
      max = input$minmax[2]
      
      if(min == max)
      {
        output$plot_poblacion_titulo = renderText({'Warning! You are choosing a single point, not an interval.'})
        output$plot_tcl_titulo = renderText({''})
        return(NULL)
      }
      runif(n = pob_size, min = min, max = max)
    }
    else if(input$dist == 'rgamma' && !is.null(input$a)&& !is.null(input$lambda))
    {
      rgamma(n = pob_size, shape = input$a, rate = input$lambda)
    }
    else if(input$dist == 'rpois'&& !is.null(input$lambda))
    {
      rpois(n = pob_size, lambda = input$lambda)
    }
    else if(input$dist == 'rexp' && !is.null(input$lambda))
    {
      rexp(n = pob_size, rate = input$lambda)
    }
    else if(input$dist == 'rgeom' && !is.null(input$p))
    {
      rgeom(n = pob_size, prob = input$p)
    }
    else if(input$dist == 'rbeta' && !is.null(input$a) && !is.null(input$b))
    {
      rbeta(n = pob_size, shape1 = input$a, shape2 = input$b)
    }
    else if(input$dist == 'rbinom' && !is.null(input$n) && !is.null(input$p))
    {
      rbinom(n = pob_size, size = input$n, prob = input$p)
    }
    else if(input$dist == 'rnorm' && !is.null(input$mu) && !is.null(input$sigma))
    {
      rnorm(n = pob_size, mean = input$mu, sd = input$sigma)
    }
    else if(input$dist == 'rchisq' && !is.null(input$df))
      rchisq(n = pob_size, df = input$df)
    
    else
    {
      return(NULL)
    }
  })
  
  # sample_mean is the CLT
  
  sample_mean = reactive({
    
    if(is.null(poblacion()))
    {
      return(NULL)
    }
    
    sample_mean = c()
    
    for(i in 1:input$repeticiones)
    {
      sample_mean = c(sample_mean, mean(sample(x = poblacion(), size = input$sample_size, replace = TRUE)))
    }
    
    if(input$dist == 'runif')
    {
      mu = (input$minmax[2] + input$minmax[1])/2
      sigma = sqrt((input$minmax[2] - input$minmax[1])^2 /12)
    }
    
    else if(input$dist == 'rgamma')
    {
      a = input$a
      lambda  = input$lambda
      mu = a/lambda
      sigma = sqrt(a/(lambda)^2)
    }
    
    else if(input$dist == 'rpois')
    {
      lambda = input$lambda
      mu = lambda
      sigma = sqrt(lambda)
    }
    
    else if(input$dist == 'rexp')
    {
      lambda = input$lambda
      mu = 1/lambda
      sigma = sqrt(1/lambda^2)
    }
    
    else if(input$dist == 'rgeom')
    {
      p = input$p
      mu = (1-p)/p
      sigma = sqrt((1-p)/(p^2))
    }
    
    else if(input$dist == 'rbeta')
    {
      a = input$a
      b = input$b
      
      mu = a / (a+b)
      sigma = sqrt((a*b) / ((a+b+1)*(a+b)^2))
    }
    
    else if(input$dist == 'rbinom')
    {
      n = input$n
      p = input$p
      mu = n*p
      sigma = sqrt(n*p*(1-p))
    }
    
    else if(input$dist == 'rnorm')
    {
      mu = input$mu
      sigma = input$sigma
    }
    
    else if(input$dist == 'rchisq')
    {
      df = input$df
      mu = df
      sigma = sqrt(2*df)
    }
    
    (sample_mean - mu)/(sigma/sqrt(input$sample_size))
    
  })
  
  
  # If the user soesn't choose nay visualization option there is nothing to show
  # If poblacion() is null there is nothing to show either.
  # Otherwise, you will see the plots with the options you chose.
    
  output$plot_poblacion = renderPlot({
    
    if(is.null(poblacion()) || length(input$hist_dens_poblacion) == 0)
    {
      return(NULL)
    }
    
    output$plot_poblacion_titulo = renderText({
      'Population Plot'
    })
    
    x = poblacion()
    
    plot = ggplot(data.frame(x), aes(x = x, y = ..density..))
    
    if('hist_plot' %in% input$hist_dens_poblacion)
    {
      plot = plot + geom_histogram(bins = 45, color = 'black', fill = 'lightblue')
    }
    
    if('dens_plot' %in% input$hist_dens_poblacion)
    {
      plot = plot +  stat_density(geom = 'line', color = 'blue', lwd = 1)
    }
    
    plot
    
  })
  
  output$plot_tcl = renderPlot({
    
    if(is.null(sample_mean()) || length(input$hist_dens_mean) == 0)
    {
      return(NULL)
    }
    
    output$plot_tcl_titulo = renderText({
      'CLT Sample Plot'
    })
    
    x = sample_mean()
    
    plot = ggplot(data.frame(x), aes(x = x, y = ..density..))
    
    if('hist_plot' %in% input$hist_dens_mean)
    {
      plot = plot + geom_histogram(bins = 45, color = 'black', fill = 'lightblue')
    }
    
    if('dens_plot' %in% input$hist_dens_mean)
    {
      plot = plot + stat_density(geom = 'line', color = 'blue', lwd = 1)
    }
    
    plot
    
  })
  
  # If the user doesn't choose any option, there will be a message.
  
  output$plot_poblacion_advertencia = renderText({

    
    if(length(input$hist_dens_poblacion) > 0)
    {
      return(NULL)
    }
    
    output$plot_poblacion_titulo = renderText({
      ''
    })
    
    'Please, choose some visualization option for the Population Plot.'
    
    
  })
  
  output$plot_tcl_advertencia = renderText({
    
    if(length(input$hist_dens_mean) > 0)
    {
      return(NULL)
    }
    
    output$plot_tcl_titulo = renderText({
      ''
    })
    
    'Please, choose some visualization option for the CLT Sample Plot.'
    
    
  }) 
  
  # With this panel you can see the Pobability / Density, Mean and Variance of the
  # distribution you chose.
  
  output$info = renderUI({
    if(input$dist == 'runif')
    {
      withMathJax(paste0('Density function :',
                         '$$f(x) = \\frac{1}{b-a} \\mathbb{1}_{[a,b]}(x), \\hspace{0.5cm} a > b$$',
                         'Mean and Variance :',
                         '$$E[X] = \\frac{b+a}{2}$$',
                         '$$V[X] = \\frac{(b-a)^2}{12}$$'))
    }
    
    else if(input$dist == 'rgamma')
    {
      withMathJax(paste0('Density function :',
                         '$$f(x) = \\frac{\\lambda^a}{\\Gamma(a)} x^{a-1} e^{-\\lambda x},
                         \\hspace{0.5cm} a, \\lambda > 0$$', 
                         'Mean and Variance :',
                         '$$E[X] = \\frac{a}{\\lambda}$$',
                         '$$V[X] = \\frac{a}{\\lambda^2}$$'))   
    }  
    
    else if(input$dist == 'rpois')
    {
      withMathJax(paste0('Probability function :',
                         '$$f(k) = \\frac{e^{-\\lambda} \\lambda ^ k}{k!}, \\hspace{0.5cm}  \\lambda > 0$$',
                         'Mean and Variance :',
                         '$$E[X] = \\lambda$$',
                         '$$V[X] = \\lambda$$'))
    }
    
    else if(input$dist == 'rexp')
    {
      withMathJax(paste0('Density function :',
                         '$$f(x) = \\lambda e^{-\\lambda x}, \\hspace{0.5cm}  \\lambda > 0$$',
                         'Mean and Variance :',
                         '$$E[X] = \\frac{1}{\\lambda}$$',
                         '$$V[X] = \\frac{1}{\\lambda^2}$$'))
    }  
    
    else if(input$dist == 'rgeom')
    {
      withMathJax(paste0('Probability function :',
                         '$$f(k) = p(1-p)^k, \\hspace{0.5cm} k = 0,1,2, ...$$',
                         'Mean and Variance :',
                         '$$E[X] = \\frac{1-p}{p}$$',
                         '$$V[X] = \\frac{1-p}{p^2}$$'))      
    }
    
    else if(input$dist == 'rbeta')
    {
      withMathJax(paste0('Density function :',
                         '$$f(x) = \\frac{\\Gamma(a+b)}{\\Gamma(a) \\Gamma(b)} x^{a-1} (1-x)^{b-1}
                         \\mathbb{1}_{[0,1]}(x)$$',
                         'Mean and Variance :',
                         '$$E[X] = \\frac{a}{a+b}$$',
                         '$$V[X] = \\frac{ab}{(a+b)^2 (a+b+1)}$$'))      
    }
    
    else if(input$dist == 'rbinom')
    {
      withMathJax(paste0('Probability function :',
                         '$$f(k) = {n \\choose k} p^k (1-p)^{(n-k)}, \\hspace{0.5cm} k = 0,1,2,..., n $$',
                         'Mean and Variance :',
                         '$$E[X] = np$$',
                         '$$V[X] = np(1-p)$$'))      
    }
    
    else if(input$dist == 'rnorm')
    {
      withMathJax(paste0('Density function :',
                         '$$f(x) = \\frac{1}{\\sigma \\sqrt{2 \\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}},
                         \\hspace{0.5cm} \\mu \\in \\mathbb{R},  \\sigma > 0$$',
                         'Mean and Variance :',
                         '$$E[X] = \\mu$$',
                         '$$V[X] = \\sigma^2$$'))      
    }
    
    else if(input$dist == 'rchisq')
    {
      withMathJax(paste0('Density function :',
                         '$$f(x) = \\frac{(1/2)^{\\nu/2}}{\\Gamma(\\nu/2)} x^{\\nu/2-1} e^{-x/2}$$',
                         'Mean and Variance :',
                         '$$E[X] = \\nu$$',
                         '$$V[X] = 2\\nu$$'))      
    }
    
  })
  
}