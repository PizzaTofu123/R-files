display.digit <- function(one_digit)
{
  names(one_digit)[1] = "label"
  for (i in 1:256) 
  { 
    names(one_digit)[i+1]=paste0("pixel",(i-1)) 
  }
  
  one_digit %>%
    melt(id.vars = "label") %>%
    mutate(pixel_value = gsub("pixel", "", variable),
           pixel_value = as.numeric(pixel_value),
           y.coord = -floor(pixel_value / 16),
           x.coord = pixel_value %% 16) %>%
    ggplot(aes(x = x.coord, y = y.coord, alpha = value))+
    geom_tile() +
    scale_alpha_continuous(range = c(0,1))
}