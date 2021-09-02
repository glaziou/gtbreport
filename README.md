# gtbreport
version 0.1.10

Utility functions, colour palettes and themes for the upcoming online Global TB Report.

## Authors: 
Philippe Glaziou, Hazim Timimi, Irwin Law


## Examples:

ftb(23456)

[1] "23 500"

ftb(c(0.0359, 0.00036))

[1] "0.036" "<0.01"

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~ cyl)

print(p)

![image](https://user-images.githubusercontent.com/233963/121515214-3ba47d80-c9ed-11eb-8b07-176f3131b616.png)


p2 <- p + theme_gtb()

print(p2)

![image](https://user-images.githubusercontent.com/233963/121515261-4b23c680-c9ed-11eb-8ec5-5f36e3088ffe.png)

