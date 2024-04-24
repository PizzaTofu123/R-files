# Simple histogram + density plot for displaying posterior distributions
# FIT3154 Studio 5, Daniel F. Schmidt
#
my.density.plot = function(x, xlabel = "", ylabel = "Density", no.hist = F, binwidth = NULL, bins = NULL)
{
  my.plot = ggplot(data=NULL, aes(x=x)) + xlab(xlabel) + ylab(ylabel)
  if (!no.hist)
  {
    my.plot = my.plot + geom_histogram(aes(y=..density..), binwidth=binwidth, bins=bins, colour="black", fill="lightblue")
  }
  my.plot = my.plot + geom_density(size=1) + theme_bw()
  my.plot
}
