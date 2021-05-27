# gtbreport
version 0.1.2

Utility functions, colour palettes and themes for the upcoming online Global TB Report.

## Authors: 
Philippe Glaziou, Hazim Timimi, Irwin Law


## Examples:

# ftb()
ftb(23456)

[1] "23 500"

ftb(c(0.0359, 0.00036))

[1] "0.036" "<0.01"

# theme_gtb()
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~ cyl)

print(p)

![image](https://user-images.githubusercontent.com/233963/119774215-20e8e980-bec2-11eb-818a-99e76043d2a9.png)


p2 <- p + theme_gtb()

print(p2)

![image](https://user-images.githubusercontent.com/233963/119774243-2a725180-bec2-11eb-8b6d-8b3f205ef6de.png)
